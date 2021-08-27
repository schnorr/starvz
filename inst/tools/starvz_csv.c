#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <zlib.h>
#include <sys/time.h>

#define CHUNK (256 * 1024)

/* report a zlib or i/o error */
void zerr(int ret)
{
    switch (ret) {
    case Z_ERRNO:
        if (ferror(stdin))
            fputs("error reading stdin\n", stderr);
        if (ferror(stdout))
            fputs("error writing stdout\n", stderr);
        break;
    case Z_STREAM_ERROR:
        fputs("invalid compression level\n", stderr);
        break;
    case Z_DATA_ERROR:
        fputs("invalid or incomplete deflate data\n", stderr);
        break;
    case Z_MEM_ERROR:
        fputs("out of memory\n", stderr);
        break;
    case Z_VERSION_ERROR:
        fputs("zlib version mismatch!\n", stderr);
    }
}

enum kind{KIND_MEMORY, KIND_COMM, KIND_WORKER, KIND_OTHER, KIND_VAR, KIND_LINK, KIND_EVENT};

struct t_out_files{
  FILE* dest[7];
  z_stream* strm[7];
};

int write_to_kind(enum kind type, struct t_out_files out_files, unsigned char* to_compress, int size){

  int ret, flush;
  unsigned have;
  unsigned char in[CHUNK];
  unsigned char out[CHUNK];

  out_files.strm[type]->avail_in = size;

  flush = size==0 ? Z_FINISH : Z_NO_FLUSH;

  out_files.strm[type]->next_in = to_compress;

  /* run deflate() on input until output buffer not full, finish
     compression if all of source has been read in */
  do {
      out_files.strm[type]->avail_out = CHUNK;
      out_files.strm[type]->next_out = out;
      ret = deflate(out_files.strm[type], flush);    /* no bad return value */
      assert(ret != Z_STREAM_ERROR);  /* state not clobbered */
      have = CHUNK - out_files.strm[type]->avail_out;
      if(have){
        if (fwrite(out, 1, have, out_files.dest[type]) != have || ferror(out_files.dest[type])) {
            printf("Error\n");
            (void)deflateEnd(out_files.strm[type]);
            return Z_ERRNO;
        }
      }
  } while (out_files.strm[type]->avail_out == 0);
  assert(out_files.strm[type]->avail_in == 0);     /* all input will be used */

  return Z_OK;
}

static int expected_commas[7] = {7, 7, 18, 7, 6, 13, 9};
static int allocs = 0;
int process_line(char* line, int size, struct t_out_files out_files){
  int pos = 0;
  int comma = 0;
  int comma1 = 0;
  while(pos < size){
    if(line[pos]==','){
      comma++;
      if(comma==2){
        comma1 = pos + 2;
      }
    }
    pos++;
  }

  int select_kind = KIND_OTHER;
  if(line[0]=='E'){
    select_kind = KIND_EVENT;
  }else if(line[0]=='V'){
    select_kind = KIND_VAR;
  }else if(line[0]=='L'){
    select_kind = KIND_LINK;
  }else if(line[0]=='S'){
    if(line[comma1]=='M'){
      select_kind = KIND_MEMORY;
    }else if(line[comma1]=='C'){
      select_kind = KIND_COMM;
    }else if(line[comma1]=='W'){
      select_kind = KIND_WORKER;
    }
  }

  char* linecpy = (char*)malloc(500*sizeof(char));
  allocs++;
  if(allocs>1000000){
    #pragma omp taskwait
  }
  memcpy(linecpy, line, size);
  #pragma omp task depend(inout: out_files.strm[select_kind]) firstprivate(select_kind, size, linecpy)
  {
    if(comma < expected_commas[select_kind]){
      write_to_kind(select_kind, out_files, linecpy, size-1);
      for(int i=comma; i<expected_commas[select_kind]; i++){
        write_to_kind(select_kind, out_files, ",", 1);
      }
      write_to_kind(select_kind, out_files, "\n", 1);
    }else{
        write_to_kind(select_kind, out_files, linecpy, size);
    }

    free(linecpy);
    #pragma omp atomic
    allocs--;
  }

  //write_to_kind(select_kind, out_files, line, size);

}

int convert(FILE* source, struct t_out_files out_files){

    int ret;
    unsigned have;
    z_stream strm;
    unsigned char in[CHUNK];
    unsigned char out[CHUNK];

    /* allocate inflate state */
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    strm.avail_in = 0;
    strm.next_in = Z_NULL;
    ret = inflateInit2(&strm, 16+MAX_WBITS);
    if (ret != Z_OK)
        return ret;

    /* decompress until deflate stream ends or end of file */
    char old_line[500];
    int old_line_s = 0;
    do {
        strm.avail_in = fread(in, 1, CHUNK, source);
        if (ferror(source)) {
            (void)inflateEnd(&strm);
            return Z_ERRNO;
        }
        if (strm.avail_in == 0)
            break;
        strm.next_in = in;

        /* run inflate() on input until output buffer not full */
        do {
            strm.avail_out = CHUNK;
            strm.next_out = out;
            ret = inflate(&strm, Z_NO_FLUSH);
            assert(ret != Z_STREAM_ERROR);  /* state not clobbered */
            switch (ret) {
            case Z_NEED_DICT:
                ret = Z_DATA_ERROR;     /* and fall through */
            case Z_DATA_ERROR:
            case Z_MEM_ERROR:
                (void)inflateEnd(&strm);
                return ret;
            }
            have = CHUNK - strm.avail_out;
            //if (fwrite(out, 1, have, dest) != have || ferror(dest)) {

            //Here is the logic to select the correct file
            int pos = 0;
            int start = 0;
            while(pos < have){

              if(out[pos]=='\n'){
                if(old_line_s){
                  memcpy(old_line + old_line_s, out, pos + 1);
                  process_line(old_line, old_line_s + pos + 1, out_files);
                  old_line_s = 0;
                }else{
                  process_line(out + start, pos - start + 1, out_files);
                }
                start = pos + 1;
              }else if(pos==(have-1)){
                old_line_s = have - start;
                memcpy(old_line, out + start, old_line_s);
              }
              pos++;
            }

        } while (strm.avail_out == 0);

        /* done when inflate() says it's done */
    } while (ret != Z_STREAM_END);

    /* clean up and return */
    (void)inflateEnd(&strm);
    return ret == Z_STREAM_END ? Z_OK : Z_DATA_ERROR;
}

/*

zgrep -e "Memory Node State" paje.csv.gz | gzip -c >> $PAJE_MEMORY_STATE

zgrep -e "Communication Thread State" paje.csv.gz | gzip -c >> $PAJE_COMM_STATE

zgrep -e "Worker State" paje.csv.gz | sed -e 's/\(State\)\(,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*$\)/\1\2,,,,,,,,,,,/g' | gzip -c >> $PAJE_WORKER_STATE

zgrep -E "^State" paje.csv.gz | grep -E -v "(Memory Node State|Communication Thread State|Worker State)" | gzip -c >> $PAJE_OTHER_STATE

zgrep -E "^Variable" paje.csv.gz | gzip -c >> $PAJEVARIABLE

zgrep -E "^Link" paje.csv.gz | sed -e 's/\(Link\)\(,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*$\)/\1\2,,,/g' | gzip -c >> $PAJELINK

zgrep -E "^Event" paje.csv.gz | sed -e 's/\(Event\)\(,[^,]*,[^,]*,[^,]*,[^,]*$\)/\1\2,,,,,/g' | sed -e 's/\(Event\)\(,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*$\)/\1\2,/g' | gzip -c >> $PAJEEVENT
*/

int main(){
  FILE* source = fopen("paje.csv.gz", "rb");
  struct t_out_files out_files;

  //Start files
  out_files.dest[KIND_MEMORY] = fopen("paje.memory_state.csv.gz", "wb");
  out_files.dest[KIND_COMM] = fopen("paje.comm_state.csv.gz", "wb");
  out_files.dest[KIND_WORKER] = fopen("paje.worker_state.csv.gz", "wb");
  out_files.dest[KIND_OTHER] = fopen("paje.other_state.csv.gz", "wb");
  out_files.dest[KIND_VAR] = fopen("paje.variable.csv.gz", "wb");
  out_files.dest[KIND_LINK] = fopen("paje.link.csv.gz", "wb");
  out_files.dest[KIND_EVENT] = fopen("paje.events.csv.gz", "wb");

  int ret;

  //Start streams
  for(int i=0; i<7; i++){
    out_files.strm[i] = (z_stream*)malloc(sizeof(z_stream));
    out_files.strm[i]->zalloc = Z_NULL;
    out_files.strm[i]->zfree = Z_NULL;
    out_files.strm[i]->opaque = Z_NULL;
    //Gzip format
    ret = deflateInit2(out_files.strm[i], Z_DEFAULT_COMPRESSION, Z_DEFLATED, 15 | 16, 8, Z_DEFAULT_STRATEGY);
    if (ret != Z_OK) return 1;
  }

  //Headers
  write_to_kind(KIND_MEMORY, out_files, "Nature, ResourceId, Type, Start, End, Duration, Depth, Value\n", 61);
  write_to_kind(KIND_COMM, out_files, "Nature, ResourceId, Type, Start, End, Duration, Depth, Value\n", 61);
  write_to_kind(KIND_WORKER, out_files, "Nature, ResourceId, Type, Start, End, Duration, Depth, Value, Size, Params, Footprint, Tag, JobId, SubmitOrder, GFlop, X, Y, Iteration, Subiteration\n", 149);
  write_to_kind(KIND_OTHER, out_files, "Nature, ResourceId, Type, Start, End, Duration, Depth, Value\n", 61);
  write_to_kind(KIND_VAR, out_files, "Nature, ResourceId, Type, Start, End, Duration, Value\n", 54);
  write_to_kind(KIND_LINK, out_files, "Nature, Container, Type, Start, End, Duration, Size, Origin, Dest, Key, Tag, MPIType, Priority, Handle\n", 103);
  write_to_kind(KIND_EVENT, out_files, "Nature, Container, Type, Start, Value, Handle, Info, Size, Tid, Src\n", 68);

  #pragma omp parallel num_threads(4)
  {
      #pragma omp single
      zerr(convert(source, out_files));
  }


  //Close everything
  fclose(source);

  for(int i=0; i<7; i++){
    write_to_kind(i, out_files, " ", 0);//This will force Flush
    (void)deflateEnd(out_files.strm[i]);
    fclose(out_files.dest[i]);
    free(out_files.strm[i]);
  }


}
