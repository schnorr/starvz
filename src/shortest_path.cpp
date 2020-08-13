// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <boost/graph/dag_shortest_paths.hpp>
#include <boost/graph/adjacency_list.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector boost_shortest_path(int s, DataFrame df) {
  //Properties
  typedef boost::property<boost::edge_weight_t, double> WeightProperty;
  typedef boost::property<boost::vertex_name_t, int> NameProperty;

  //Graph Type
  typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS, NameProperty, WeightProperty> Graph;

  //Vertex Descriptor
  typedef boost::graph_traits<Graph>::vertex_descriptor Vertex;

  //The IndexMap
  typedef boost::property_map<Graph,boost::vertex_index_t>::type IndexMap;

  //The Predecessor and Distance Maps
  typedef boost::iterator_property_map <Vertex*, IndexMap, Vertex, Vertex& > PredecessorMap;
  typedef boost::iterator_property_map <double*, IndexMap, double, double& > DistanceMap;

  //Graph Instance
  Graph g1;

  //Create the Vertices
  std::map<int, Vertex> VertexMap;
  std::map<Vertex, int> JobIdMap;
  IntegerVector JobId = df["JobId"];
  for (int i = 0; i < JobId.size(); i++) {
    if (VertexMap.find(JobId[i]) == VertexMap.end()) {
      Vertex v = boost::add_vertex(JobId[i], g1);
      VertexMap[JobId[i]] = v;
      JobIdMap[v] = JobId[i];
    }
  }
  IntegerVector Dependent = df["Dependent"];
  for (int i = 0; i < Dependent.size(); i++) {
    if (VertexMap.find(Dependent[i]) == VertexMap.end()) {
      Vertex v = boost::add_vertex(Dependent[i], g1);
      VertexMap[Dependent[i]] = v;
      JobIdMap[v] = Dependent[i];
    }
  }
  //Get reference to starting vertex
  Vertex source = VertexMap.find(s)->second;

  //Create the Edges with the associated Cost
  NumericVector Cost = df["Cost"];
  for (int i = 0; i < JobId.size(); i++) {
     Vertex from = VertexMap.find(Dependent[i])->second;
     Vertex to = VertexMap.find(JobId[i])->second;
     boost::add_edge(from, to, Cost[i], g1);
  }

  // Create things necessary to calculate the costlier shortest path
  std::vector<Vertex> predecessors(boost::num_vertices(g1)); // To store parents
  std::vector<double> distances(boost::num_vertices(g1)); // To store distances

  IndexMap indexMap = boost::get(boost::vertex_index, g1);
  PredecessorMap predecessorMap(&predecessors[0], indexMap);
  DistanceMap distanceMap(&distances[0], indexMap);

  // Call the Function
  boost::dag_shortest_paths(g1, source, boost::distance_map(distanceMap).predecessor_map(predecessorMap));

  // Find out the costlier path
  Vertex vtemp;
  double cost = std::numeric_limits<double>::max();
  boost::graph_traits<Graph>::vertex_iterator vi, vi_end;
  for (boost::tie(vi, vi_end) = boost::vertices(g1); vi != vi_end; ++vi){
    if (distanceMap[*vi] < cost) {
      vtemp = *vi;
      cost = distanceMap[*vi];
    }
  }

  // Get the path
  NumericVector ret;
  while (vtemp != source){
    ret.push_back(JobIdMap[vtemp]);
    vtemp = predecessorMap[vtemp];
  }
  ret.push_back(JobIdMap[source]);
  return ret;
}
