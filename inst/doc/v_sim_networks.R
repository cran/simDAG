## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>",
  fig.align="center"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(data.table)
library(igraph)
library(simDAG)

## ----fig.width=6, fig.height=6------------------------------------------------
set.seed(1234)

data <- data.frame(from=c(1, 1, 2, 3, 4, 5, 5),
                   to=c(2, 3, 4, 4, 1, 3, 4))

g <- graph_from_data_frame(data, directed=FALSE)
plot(g)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=10) +
  node("sex", type="rbernoulli", p=0.5, output="numeric")

## -----------------------------------------------------------------------------
set.seed(5245)
data <- sim_from_dag(dag, n_sim=5)
print(data, row.names=TRUE)

## -----------------------------------------------------------------------------
dag <- dag + network("network1", net=g)

## -----------------------------------------------------------------------------
dag <- dag + node("infected", type="binomial",
                  formula= ~ 5 + age*0.1 + sex*-0.5 +
                    net(mean(age))*-0.2 + net(mean(sex))*-0.4)

## -----------------------------------------------------------------------------
set.seed(5245)
data <- sim_from_dag(dag, n_sim=5)
print(data, row.names=TRUE)

## -----------------------------------------------------------------------------
dag <- dag + node("n_inf_neighbors", type="identity",
                  formula= ~ net(sum(infected)), kind="data")

set.seed(5245)
data <- sim_from_dag(dag, n_sim=5)
print(data, row.names=TRUE)

## -----------------------------------------------------------------------------
set.seed(56356)
g1 <- igraph::sample_gnm(n=20, m=30)
g2 <- igraph::sample_gnm(n=20, m=30)

V(g1)$color <- "salmon"
V(g2)$color <- "lightblue"

## ----fig.width=7, fig.height=5------------------------------------------------
par(mfrow=c(1, 2), mar=c(0, 0, 0, 0) + 1)

plot(g1, main="Friends", margin=0, vertex.label.cex=0.8)
plot(g2, main="Work", margin=0, vertex.label.cex=0.8)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=10) +
  node("sex", type="rbernoulli", p=0.5, output="numeric") +
  network("friends", net=g1) +
  network("work", net=g2)

## -----------------------------------------------------------------------------
dag <- dag + node("infected", type="binomial",
                  formula= ~ 3 + age*0.1 + sex*-0.5 +
                    net(mean(age), net="friends")*-0.2 + 
                    net(mean(sex), net="work")*-0.4)

## -----------------------------------------------------------------------------
data <- sim_from_dag(dag, n_sim=20)
head(data, row.names=TRUE)

## ----fig.width=6, fig.height=6------------------------------------------------
E(g1)$weight <- runif(n=length(E(g1)), min=1, max=5)
plot(g1, edge.width=E(g1)$weight)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  network("friends", net=g1) +
  node("age", type="rnorm", mean=50, sd=10) +
  node("sex", type="rbernoulli", p=0.5, output="numeric") +
  node("infected", type="binomial",
       formula= ~ 3 + age*0.1 + sex*-0.5 +
                  net(weighted.mean(x=age, w=..weight..))*-0.2 + 
                  net(weighted.mean(x=sex, w=..weight..))*-0.4)
data <- sim_from_dag(dag, n_sim=20)
head(data)

## ----fig.width=6, fig.height=6------------------------------------------------
set.seed(123)

g <- sample_gnm(n=20, m=18, directed=TRUE, loops=FALSE)
plot(g, edge.arrow.size=0.4)

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  network("work", net=g) +
  node("age", type="rnorm", mean=50, sd=10) +
  node("sex", type="rbernoulli", p=0.5, output="numeric") +
  node("infected", type="binomial",
       formula= ~ 3 + age*0.1 + sex*-0.5 +
                  net(mean(age), mode="out", na=0)*-0.2 + 
                  net(mean(sex), mode="out", na=0)*-0.4)
data <- sim_from_dag(dag, n_sim=20)
head(data)

## ----fig.width=7, fig.height=5, echo=FALSE------------------------------------
set.seed(32346)
g1 <- g2 <- igraph::sample_gnm(n=20, m=25)

# order = 1
neighbors <- neighbors(g1, v=11)
V(g1)$color <- ifelse(V(g1) %in% neighbors, "salmon", "lightblue")
V(g1)$color[11] <- "green"

# order = 2
neighbors <- neighborhood(g2, nodes=11, order=2)
V(g2)$color <- ifelse(V(g2) %in% as.numeric(neighbors[[1]]),
                      "salmon", "lightblue")
V(g2)$color[11] <- "green"

par(mfrow=c(1, 2), mar=c(0, 0, 0, 0) + 1)
set.seed(123)
plot(g1, main="order = 1", margin=0, vertex.label.cex=0.8)
set.seed(123)
plot(g2, main="order = 2", margin=0, vertex.label.cex=0.8)

## -----------------------------------------------------------------------------
set.seed(2134)
g <- sample_gnm(n=20, m=15)

dag <- empty_dag() +
  network("net1", net=g) +
  node("age", type="rnorm", mean=50, sd=10) +
  node("sex", type="rbernoulli", p=0.5, output="numeric") +
  node("infected", type="binomial",
       formula= ~ 3 + age*0.1 + sex*-0.5 +
                  net(mean(age), order=1, na=0)*-0.2 + 
                  net(mean(sex), order=2, na=0)*-0.4)
data <- sim_from_dag(dag, n_sim=20)
head(data)

## ----fig.width=7, fig.height=5, echo=FALSE------------------------------------
# order = 2, mindist = 1
neighbors <- neighborhood(g1, nodes=11, order=2, mindist=1)
V(g1)$color <- ifelse(V(g1) %in% as.numeric(neighbors[[1]]),
                      "salmon", "lightblue")
V(g1)$color[11] <- "green"

# order = 2, mindist = 2
neighbors <- neighborhood(g2, nodes=11, order=2, mindist=2)
V(g2)$color <- ifelse(V(g2) %in% as.numeric(neighbors[[1]]),
                      "salmon", "lightblue")
V(g2)$color[11] <- "green"

par(mfrow=c(1, 2), mar=c(0, 0, 0, 0) + 1)
set.seed(123)
plot(g1, main="order = 2, mindist = 1", margin=0, vertex.label.cex=0.8)
set.seed(123)
plot(g2, main="order = 2, mindist = 2", margin=0, vertex.label.cex=0.8)

## -----------------------------------------------------------------------------
set.seed(2134)
g <- sample_gnm(n=20, m=15)

dag <- empty_dag() +
  network("net1", net=g) +
  node("age", type="rnorm", mean=50, sd=10) +
  node("sex", type="rbernoulli", p=0.5, output="numeric") +
  node("infected", type="binomial",
       formula= ~ 3 + age*0.1 + sex*-0.5 +
                  net(mean(age), order=2, mindist=2, na=0)*-0.4)
data <- sim_from_dag(dag, n_sim=20)
head(data)

## -----------------------------------------------------------------------------
is_different_sex <- function(g, x) {
  V(g)[ends(g, x)[1]]$type != V(g)[ends(g, x)[2]]$type
}

gen_network <- function(n_sim, data) {
  g <- sample_gnm(n=n_sim, m=50)
  V(g)$type <- data$sex
  g <- delete_edges(g, which(vapply(E(g), is_different_sex,
                                    FUN.VALUE=logical(1), g=g)))
  return(g)
}

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node("age", type="rnorm", mean=25, sd=5) +
  node("sex", type="rbernoulli", p=0.5) +
  network("network1", net=gen_network, parents="sex") +
  node("infected", type="binomial", formula= ~ 1 + net(mean(age))*0.5)

## -----------------------------------------------------------------------------
set.seed(1324)
data <- sim_from_dag(dag, n_sim=20, return_networks=TRUE)
head(data$data)

## ----fig.width=6, fig.height=6------------------------------------------------
g <- data$networks$network1$net
V(g)$color <- ifelse(V(g)$type, "salmon", "lightblue")
plot(g)

## ----fig.width=6, fig.height=6------------------------------------------------
set.seed(244368)
g2 <- igraph::sample_gnm(n=18, m=30)

plot(g2)

## -----------------------------------------------------------------------------
prob_infection <- function(data, sim_time) {
  if (sim_time==1) {
    p <- rep(0.05, nrow(data))
  } else {
    p <- fifelse(data$n_infected_neighbors==0, 0,
                 fifelse(data$n_infected_neighbors > 3, 0.9, 0.4))
  }
  return(p)
}

dag <- empty_dag() +
  network("net1", net=g2) +
  node_td("n_infected_neighbors", type="identity",
          formula= ~ net(sum(infected_event), na=0), kind="data") +
  node_td("infected", type="time_to_event", event_duration=Inf,
          immunity_duration=Inf, parents=("n_infected_neighbors"),
          prob_fun=prob_infection)

## -----------------------------------------------------------------------------
sim <- sim_discrete_time(dag, n_sim=18, max_t=6, save_states="all")
data <- sim2data(sim, to="long")
head(data)

## ----fig.width=7, fig.height=9------------------------------------------------
E(g2)$color <- "lightgray"

par(mfrow=c(3, 2), mar=c(0, 0, 0, 0) + 2)

for (i in seq_len(6)) {
  
  data_i <- subset(data, .time==i)
  
  V(g2)$color <- ifelse(data_i$infected, "salmon", "lightblue")
  
  set.seed(124)
  plot(g2,
       vertex.label.cex=0.8,
       vertex.label.color="black",
       size=1,
       main=paste0("t = ", i),
       layout=layout_nicely(g2),
       margin=0)
}

## -----------------------------------------------------------------------------
gen_network <- function(n_sim) {
  igraph::sample_gnm(n=n_sim, m=30)
}

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node_td("n_infected_neighbors", type="identity",
          formula= ~ net(sum(infected_event), na=0), kind="data") +
  node_td("infected", type="time_to_event", event_duration=Inf,
          immunity_duration=Inf, parents=("n_infected_neighbors"),
          prob_fun=prob_infection) +
  network_td("net1", net=gen_network)

set.seed(1335)

sim <- sim_discrete_time(dag, n_sim=18, max_t=6, save_states="all",
                         save_networks=TRUE)
data <- sim2data(sim, to="long")
head(data)

## ----fig.width=7, fig.height=9------------------------------------------------
par(mfrow=c(3, 2), mar=c(0, 0, 0, 0) + 2)

for (i in seq_len(6)) {
  
  data_i <- subset(data, .time==i)
  g_i <- sim$past_networks[[i]]$net1$net
  
  E(g_i)$color <- "lightgray"
  V(g_i)$color <- ifelse(data_i$infected, "salmon", "lightblue")
  
  set.seed(124)
  plot(g_i, vertex.label.cex=0.8,
       vertex.label.color="black",
       size=1,
       main=paste0("t = ", i),
       layout=layout_nicely(g_i),
       margin=0)
}

## -----------------------------------------------------------------------------
gen_network <- function(n_sim, sim_time, network, data) {
  
  if (sim_time==0) {
    return(igraph::sample_gnm(n=n_sim, m=23))
  }
  
  rm_edges <- data$.id[data$infected_event==TRUE &
                       data$infected_time_since_last > 0]
  
  if (length(rm_edges) > 0) {
    rm_edges <- do.call(c, incident_edges(network, rm_edges))
    g_new <- delete_edges(network, rm_edges)
  } else {
    g_new <- network
  }
  return(g_new)
}

## -----------------------------------------------------------------------------
dag <- empty_dag() +
  node_td("n_infected_neighbors", type="identity",
          formula= ~ net(sum(infected_event), na=0), kind="data") +
  node_td("infected", type="time_to_event", event_duration=Inf,
          immunity_duration=Inf, parents=("n_infected_neighbors"),
          prob_fun=prob_infection, time_since_last=TRUE) +
  network_td("net1", net=gen_network, create_at_t0=TRUE)

## -----------------------------------------------------------------------------
set.seed(13354)

sim <- sim_discrete_time(dag, n_sim=18, max_t=6, save_states="all",
                         save_networks=TRUE)
data <- sim2data(sim, to="long")
head(data)

## ----fig.width=7, fig.height=9------------------------------------------------
par(mfrow=c(3, 2), mar=c(0, 0, 0, 0) + 2)

layout_g <- layout_nicely(sim$past_networks[[1]]$net1$net)

for (i in seq_len(6)) {
  
  data_i <- subset(data, .time==i)
  g_i <- sim$past_networks[[i]]$net1$net
  
  E(g_i)$color <- "lightgray"
  V(g_i)$color <- ifelse(data_i$infected, "salmon", "lightblue")
  
  set.seed(124)
  plot(g_i, vertex.label.cex=0.8,
       vertex.label.color="black",
       size=1,
       main=paste0("t = ", i),
       layout=layout_g,
       margin=0)
}

