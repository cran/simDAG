\name{dag_from_data}
\alias{dag_from_data}

\title{
Fills a partially specified \code{DAG} object with parameters estimated from reference data
}
\description{
Given a partially specified \code{DAG} object, where only the \code{name}, \code{type} and the \code{parents} are specified plus a \code{data.frame} containing realizations of these nodes, return a fully specified \code{DAG} (with beta-coefficients, intercepts, errors, ...). The returned \code{DAG} can be used directly to simulate data with the \code{\link{sim_from_dag}} function.
}
\usage{
dag_from_data(dag, data, return_models=FALSE, na.rm=FALSE)
}
\arguments{
  \item{dag}{
A partially specified \code{DAG} object created using the \code{\link{empty_dag}} and \code{\link{node}} functions. See \code{?node} for a more detailed description on how to do this. All nodes need to contain information about their \code{name}, \code{type} and \code{parents}. All other attributes will be added (or overwritten if already in there) when using this function. Currently does not support DAGs with time-dependent nodes added with the \code{\link{node_td}} function.
  }
  \item{data}{
A \code{data.frame} or \code{data.table} used to obtain the parameters needed in the \code{DAG} object. It needs to contain a column for every node specified in the \code{dag} argument.
  }
  \item{return_models}{
Whether to return a list of all models that were fit to estimate the information for all child nodes (elements in \code{dag} where the \code{parents} argument is not \code{NULL}).
  }
  \item{na.rm}{
Whether to remove missing values or not.
  }
}
\details{

\strong{\emph{How it works}}:

It can be cumbersome to specify all the node information needed for the simulation, especially when there are a lot of nodes to consider. Additionally, if data is available, it is natural to fit appropriate models to the data to get an empirical estimate of the node information for the simulation. This function automates this process. If the user has a reasonable DAG and knows the node types, this is a very fast way to generate synthetic data that corresponds well to the empirical data.

All the user has to do is create a minimal \code{DAG} object including only information on the \code{parents}, the \code{name} and the node \code{type}. For root nodes, the required distribution parameters are extracted from the data. For child nodes, regression models corresponding to the specified \code{type} are fit to the data using the \code{parents} as independent covariates and the \code{name} as dependent variable. All required information is extracted from these models and added to the respective node. The output contains a fully specified \code{DAG} object which can then be used directly in the \code{\link{sim_from_dag}} function. It may also include a list containing the fitted models for further inspection, if \code{return_models=TRUE}.

\strong{\emph{Supported root node types}}:

Currently, the following root node types are supported:

\itemize{
  \item{\code{"rnorm"}: Estimates parameters of a normal distribution.}
  \item{\code{"rbernoulli"}: Estimates the \code{p} parameter of a Bernoulli distribution.}
  \item{\code{"rcategorical"}: Estimates the class probabilities in a categorical distribution.}
}

Other types need to be implemented by the user.

\strong{\emph{Supported child node types}}:

Currently, the following child node types are supported:

\itemize{
  \item{\code{"gaussian"}: Estimates parameters for a node of type \code{"\link[=node_gaussian]{gaussian}"}}.
  \item{\code{"binomial"}: Estimates parameters for a node of type \code{"\link[=node_binomial]{binomial}"}}.
  \item{\code{"poisson"}: Estimates parameters for a node of type \code{"\link[=node_poisson]{poisson}"}}.
  \item{\code{"negative_binomial"}: Estimates parameters for a node of type \code{"\link[=node_negative_binomial]{negative_binomial}"}}.
  \item{\code{"conditional_prob"}: Estimates parameters for a node of type \code{"\link[=node_conditional_prob]{conditional_prob}"}}.
}

Other types need to be implemented by the user.

\strong{\emph{Support for custom nodes}}:

The \code{sim_from_dag} function supports custom node functions, as described in the associated vignette. It is impossible for us to directly support these custom types in this function directly. However, the user can extend this function easily to accommodate any of his/her custom types. Similar to defining a custom node type, the user simply has to write a function that returns a correctly specified \code{node.DAG} object, given the named arguments \code{name}, \code{parents}, \code{type}, \code{data} and \code{return_model}. The first three arguments should simply be added directly to the output. The \code{data} should be used inside your function to fit a model or obtain the required parameters in some other way. The \code{return_model} argument should control whether the model should be added to the output (in a named argument called \code{model}). The function name should be \code{paste0("gen_node_", YOURTYPE)}. An examples is given below.

\strong{\emph{Interactions & cubic terms}}:

This function currently does not support the usage of interaction effects or non-linear terms (such as using \code{A ~ B + I(B^2)} as a formula). Instead, it will be assumed that all values in \code{parents} have a linear effect on the respective node. For example, using \code{parents=c("A", "B")} for a node named \code{"C"} will use the formula \code{C ~ A + B}. If other behavior is desired, users need to integrate this into their own custom function as described above.

}
\value{
A list of length two containing the new fully specified \code{DAG} object named \code{dag} and a list of the fitted models (if \code{return_models=TRUE}) in the object named \code{models}.
}
\author{
Robin Denz
}
\examples{
library(simDAG)

set.seed(457456)

# get some example data from a known DAG
dag <- empty_dag() +
  node("death", type="binomial", parents=c("age", "sex"), betas=c(1, 2),
       intercept=-10) +
  node("age", type="rnorm", mean=10, sd=2) +
  node("sex", parents="", type="rbernoulli", p=0.5) +
  node("smoking", parents=c("sex", "age"), type="binomial",
       betas=c(0.6, 0.2), intercept=-2)

data <- sim_from_dag(dag=dag, n_sim=1000)

# suppose we only know the causal structure and the node type:
dag <- empty_dag() +
  node("death", type="binomial", parents=c("age", "sex")) +
  node("age", type="rnorm") +
  node("sex", type="rbernoulli") +
  node("smoking", type="binomial", parents=c("sex", "age"))

# get parameter estimates from data
dag_full <- dag_from_data(dag=dag, data=data)

# can now be used to simulate data
data2 <- sim_from_dag(dag=dag_full$dag, n_sim=100)
}
