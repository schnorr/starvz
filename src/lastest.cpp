#include <Rcpp.h>
#include <stdio.h>
#include <vector>
#include <list>
#include <set>
#include <limits>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame lastest_task_c(DataFrame df) {
  //Get information of the DAG table
  IntegerVector jobs = df["JobId"];
  IntegerVector Dependent = df["Dependent"];
  NumericVector End = df["End"];

  CharacterVector jobs_levs = jobs.attr("levels");

  //The number of possible jobs plus one global start/genesis for all
  long int njobs = jobs_levs.size()+1;

  //A double list graph structured
  std::vector<std::set<int>> graph_Depen(njobs, std::set<int>());
  std::vector<std::set<int>> graph_next(njobs, std::set<int>());

  //This is per node and not per edge as in DAG
  std::vector<double> graph_End(njobs);
  std::vector<int> graph_Jobs(njobs);
  std::vector<int> graph_lastJobId(njobs);
  std::vector<double> graph_last(njobs);

  //A node is initialise if there is a edge from it, and not only to it
  std::vector<int> graph_init(njobs);

  //Create graph structure
  for(int i=0; i<jobs.size(); i++){
    int current_job = jobs[i];
    graph_Jobs[current_job] = current_job;
    //If it is a valid dependent
    if(!IntegerVector::is_na(Dependent[i])){
      graph_Depen[current_job].insert(Dependent[i]);
      graph_next[Dependent[i]].insert(current_job);
    }else{
      //If not say that it depends on the imaginary job 0
      graph_next[0].insert(current_job);
      graph_Depen[current_job].insert(0);
    }
    graph_End[current_job] = End[i];
    graph_init[current_job] = 1;
  }

  //Set job 0
  graph_init[0] = 1;
  graph_End[0] = 0;//This is 0 for a safe comparison of R real and double lowest

  //List of jobs that are intermediary on DAG (dont have end information)
  std::list<int> the_nas(0);

  //Mark intermediary tasks and remove dependencies not initialized
  for(int i=(njobs-1); i>=0; i--){
    graph_last[i]=std::numeric_limits<double>::lowest();
    //This job was not initialized, i.e. it does not has any dependency from it
    //However, it also does not have the dependency NA->JOB
    //This can be caused by internal preliminar tasks, or a DAG that is not fully connected
    //Anyway, lets remove it and replace this dependency of other jobs to 0
    if(graph_init[i]==0){
      graph_next[0].insert(i);
      std::set<int> next = graph_next[i];
      for(auto nn = next.begin(); nn != next.end(); ++nn) {
        graph_Depen[*nn].erase(i);
        graph_Depen[*nn].insert(0);
        graph_next[0].insert(*nn);
      }
    }
    // Add the nas to a list
    if(NumericVector::is_na(graph_End[i])){
      the_nas.push_back(i);
    }
  }

  // Process in hope that all dependencies are here
  while(the_nas.size()>0){
    int front = the_nas.front();
    the_nas.pop_front();

    std::set<int> deps = graph_Depen[front];
    std::set<int> next = graph_next[front];

    //Clean DAG from NA Tasks
    for(auto dep = deps.begin(); dep != deps.end(); ++dep) {
      graph_next[*dep].erase(front);
      for(auto nn = next.begin(); nn != next.end(); ++nn) {
	      graph_next[*dep].insert(*nn);
        graph_Depen[*nn].insert(*dep);
        graph_Depen[*nn].erase(front);
      }
    }
    graph_Depen[front].clear();
    graph_next[front].clear();
  }

  //Lets compute the lastest path for all nodes
  the_nas.push_back(0); //Start from the commom start-point imaginary task

  while(the_nas.size()>0){
    int front = the_nas.front();

    the_nas.pop_front();

    std::set<int> next = graph_next[front];

    for(auto dep = next.begin(); dep != next.end(); ++dep) {
      if(graph_init[*dep] == 1){
      	if(graph_last[*dep] < graph_End[front]){
      	  graph_lastJobId[*dep] = front;
      	  graph_last[*dep] = graph_End[front];
      	  the_nas.push_back(*dep);
      	}
      }
    }
  }

  //Lets clean the factors used and remap it
  std::vector<int> factor_used(njobs);

  int used=1;
  for(int i=0; i<njobs; i++){
    if(graph_End[i]!=-1 && graph_Jobs[i]>0 && !factor_used[graph_Jobs[i]]){
      factor_used[graph_Jobs[i]] = used++;
    }
  }

  CharacterVector new_jobs_levs(used-1);

  for(int i=0; i<njobs; i++){
    if(factor_used[graph_Jobs[i]]){
      new_jobs_levs[factor_used[graph_Jobs[i]]-1] = jobs_levs[graph_Jobs[i]-1];
    }
  }

  IntegerVector ret_Jobs(used-1);
  NumericVector ret_End(used-1);
  IntegerVector ret_lastJobId(used-1);
  NumericVector ret_last(used-1);

  //Creates the output structured with the new factor list
  for(int i=0; i<njobs; i++){
    if(factor_used[graph_Jobs[i]]>0){
      int new_id = factor_used[graph_Jobs[i]]-1;
      ret_Jobs[new_id] = new_id+1;
      ret_End[new_id] = graph_End[i];
      if(graph_lastJobId[i]==0){
        ret_lastJobId[new_id] = NA_INTEGER;
      }else if(factor_used[graph_lastJobId[i]]==0){
        ret_lastJobId[new_id] = NA_INTEGER;
      }else{
        ret_lastJobId[new_id] = factor_used[graph_lastJobId[i]];
      }

      ret_last[new_id] = graph_last[i];
    }
  }


  ret_Jobs.attr("class") = "factor";
  ret_Jobs.attr("levels") = new_jobs_levs;

  ret_lastJobId.attr("class") = "factor";
  ret_lastJobId.attr("levels") = new_jobs_levs;

  DataFrame ret = DataFrame::create( Named("JobId") = ret_Jobs,
				     _["End"] = ret_End,
				     _["Lastest"] = ret_lastJobId,
				     _["LastEnd"] = ret_last);

  return ret;

}

// [[Rcpp::export]]
List get_last_path(DataFrame lasttask, CharacterVector selected_tasks){
  IntegerVector jobs = lasttask["JobId"];
  IntegerVector lastest = lasttask["Lastest"];

  CharacterVector jobs_levs = jobs.attr("levels");

  int ns = selected_tasks.size();

  IntegerVector selected(ns);

  CharacterVector* results = new CharacterVector[ns];

  for(int i=0;i<jobs_levs.size();i++){
     for(int y=0;y<ns;y++){
        if(jobs_levs[i]==selected_tasks[y]){
           selected[y] = i;
        }
     }
  }

  List ret = List::create();

  for(int i=0;i<ns;i++){
    int node = selected[i];
    std::list<int> the_lasts;
    while(node!=NA_INTEGER){
        the_lasts.push_back(node);
        node =  lastest[node];
        if(node!=NA_INTEGER)node--;
    }
    results[i] = CharacterVector(the_lasts.size());
    int x = 0;
    for(auto dep = the_lasts.begin(); dep != the_lasts.end(); ++dep) {
       results[i][x++] = jobs_levs[*dep];
    }
    the_lasts.clear();

    ret.push_back(results[i]);
  }

  return(ret);
}
