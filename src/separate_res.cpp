#include <Rcpp.h>
#include <stdio.h>
#include <string>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame separate_res(DataFrame data){
  IntegerVector res = data["ResourceId"];
  CharacterVector res_levels = res.attr("levels");

  int size = res.size();

  int lvls = res_levels.size();

  IntegerVector lvl_Node(lvls);
  CharacterVector lvl_Reso(lvls);

  for(int i=0; i<lvls; i++){
    std::string x = Rcpp::as<std::string>(res_levels[i]);
    size_t pos = x.find("_");
    // Single Node
    if(pos == std::string::npos){
      lvl_Node[i] = 0;
      lvl_Reso[i] = x;
    }else{ //Multi-Node
      lvl_Node[i] = std::stoi(x.substr(0, pos));
      if(pos+1 == x.size()){
        lvl_Reso[i] = NA_STRING;
      }else{
        lvl_Reso[i] = x.substr(pos+1, x.size());
      }
    }
  }

  IntegerVector Node(size);
  CharacterVector Reso(size);

  for(int i=0; i<size; i++){
    Node[i] = lvl_Node[res[i]-1];
    Reso[i] = lvl_Reso[res[i]-1];
  }

  //Add to data frame
  data["Node"] = Node;
  data["Resource"] = Reso;
  return(data);
}
