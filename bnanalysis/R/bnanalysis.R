#' Grow-Shrink Analysis
#'
#' This function applies the grow-shrink algorithm to a given data set and 
#' returns missing node connections and a model summary
#' @param data set
#' @return model summary, undirected arcs, model structure as plot
#' @export
#' gsanalysis()
gsanalysis = function(x){
  gs.model = bnlearn::gs(iris)
  model.summary = summary(gs.model)
  missing.connections = bnlearn::undirected.arcs(gs.model)
  model.display = plot(gs.model, main='Grow-Shrink Algorithm', highlight=missing.connections)
  
  result = c(model.summary, missing.connections)
  return(result)
}



#' Hill-Climbing Analysis
#'
#' This function applies the hill-climbing algorithm to a given data set and 
#' returns missing node connections and a model summary
#' @param data set
#' @return model summary, undirected arcs, model structure as plot
#' @export
#' hcanalysis()
hcanalysis = function(x){
  hc.model = bnlearn::hc(iris)
  model.summary = summary(hc.model)
  missing.connections = bnlearn::undirected.arcs(hc.model)
  model.display = plot(hc.model, main='Hill-Climbing Algorithm', highlight=missing.connections)
  
  result = c(model.summary, missing.connections)
  return(result)
}
