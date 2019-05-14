# TreeAddND
#
# This packages processes Python SKLEARN classification trees and adds a new option: undetermined.
# Undetermined nodes are nodes in which the fraction of the minor class is larger than <hetero_threshold>
# Another outcome is trimming: if a node is "pure enough", don't go further down the tree.
#
# Originally created by Eitan Rubin, erubin@bgu.ac.il
#
# Version 0.0: started the project from code I had as a stand-alone script
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# In creating this package, I followed the instuctions in "https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/"
#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param tree_text this holds the tree as a vector of text. See package manual for suitable input
#' @param class_weights this holds the weight of the different classes. Must be a numerical vector of length 2 of
#' @param maximal_allowed_heterogeneity this is the core of this function: if an inner node has less than this value, it is converted to a terminal node. Every termimal node that is not "pure" - that is, the faction of the less common class is above this threshold, it converted to "nd" status, whcih means it cannot be called
#' @param verbose should
#' @keywords decsions_trees
#' @export
#' @examples
#' WND_process_tree(tree_text,class_weights,maximal_allowed_heterogeneity,verbose)
#'

WND_process_tree<-function(tree_text,class_weights,maximal_allowed_heterogeneity,verbose=FALSE) {

  nd.tree <- NULL
  chosen.params <- NULL

  parse_node <- function(s) {
    # parse string of a node (leaf or internal)

    ar <- unlist(strsplit(s,"\\\\n"))
    value.line = grep(ar,pattern="value = ",value=T)
    value.line = sub(value.line,pattern=".*\\[",replacement="")
    value.line = sub(value.line,pattern="\\]",replacement="")
    values <- as.numeric(unlist(strsplit(value.line,", ")))
    values[1]<-values[1]/class0_weight
    values[2]<-values[2]/class1_weight
    cond.line = grep(ar,pattern="[\\<\\>]",value=T)
    if (! is.null(cond.line)) {
      cond.line=sub(cond.line,pattern=" \\[label=.",replacement=",")
      cond = unlist(strsplit(cond.line,","))
    } else {
      cond=c(NA,NA)
    }

    leaf=F
    decision = NA
    if (is.null(cond) || min(values)/sum(values)<maximal_allowed_heterogeneity) {
      leaf=T
      decision = NA
      if (min(values)/sum(values)<maximal_allowed_heterogeneity) {
        decision=c(0,1)[which.min(values)]
      }
    }
    return(list(cond=cond,vals=values,leaf=leaf,decision=decision))
  }

  follow_tree <- function(node_lines,edge_lines,start,rules,verbose) {
    this.node <- node_lines[start+1] # start is in zero offset, but arrays in R are 1-offset\\\\\\\

    if (sum(start == edge_lines[,1])>2) { # only a mistake or a bug would lead to the same edge leading to 2 nodes
      stop("ERROR %d links for node %d",sum(edge_lines[,1]==start),start)
    }

    node.data <- parse_node(this.node)
    if (node.data$leaf) { # this start point is a final edge; it does not appear as the left node in any edge
      ruleset <- paste(rules,collapse=" * ")
      nd.tree <<- rbind(nd.tree,c(ruleset,node.data$decision))
      return()
    } else {
      new.node.1<-edge_lines[edge_lines[,1]==start,2][1]
      new.node.2<-edge_lines[edge_lines[,1]==start,2][2]

      follow_tree(node_lines,edge_lines,new.node.1,c(rules,node.data$cond[2]),verbose)
      r2<-c(rules,reverse_condition(node.data$cond[2]))
      follow_tree(node_lines,edge_lines,new.node.2,r2,verbose)
    }
  }

  reverse_condition <- function(s) {
    print(s)
    if (grepl(s,pattern="<=")) {
      return(sub(s,pattern="<=",replacement=">"))
    }
    if (grepl(s,pattern=">=")) {
      return(sub(s,pattern=">=",replacement="<"))
    }
    if (grepl(s,pattern=">")) {
      return(sub(s,pattern=">",replacement="<="))
    }
    if (grepl(s,pattern="<")) {
      return(sub(s,pattern="<",replacement=">="))
    }
  }

  class0_weight <- class_weights[1]
  class1_weight <- class_weights[2]

  node_lines <- grep(tree_text,pattern="label",value=T)
  node_lines <- grep(tree_text,pattern="gini",value=T)
  edge_lines.raw <- grep(tree_text,pattern="->",value=T)
  edge_lines <- NULL

  for (edge_line.num in 1:length(edge_lines.raw)) {
    this.edge.raw<- strsplit(edge_lines.raw[edge_line.num],split="->")
    this.edge.1 <- sub(this.edge.raw[[1]][1],pattern=" \\[.*",replacement="")
    this.edge.1 <- as.numeric(this.edge.1)
    this.edge.2 <- sub(this.edge.raw[[1]][2],pattern=" \\[.*",replacement="")
    this.edge.2 <- sub(this.edge.2,pattern=" \\;.*",replacement="")
    this.edge.2 <- as.numeric(this.edge.2)
    edge_lines <- rbind(edge_lines, c(this.edge.1,this.edge.2))
  }
  follow_tree(node_lines,edge_lines,0,NULL,verbose)

  all.rules <- unlist(strsplit(nd.tree[,1]," \\* "))
  chosen.params<-sub(all.rules,pattern=" [<=>]+ .*",replacement="")
  print(all.rules)
  print(chosen.params)

  return(list(
      table=nd.tree,
      chosen.params=unique(gsub(chosen.params,pattern='-',replacement='_'))
   )
  )
}
