# ALY6015 Module 5 Code – Nonparametric Methods and Simulation
# Author: Eric Schaefer
# CRN: 81418

# 1. Install and load required packages reproducibly
required_pkgs <- c("stats", "dplyr")
for(pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# --------------------------------------------------------------------------------
# Task 1: Game Attendance – Sign test for median = 3000
# --------------------------------------------------------------------------------
attendance <- c(6210,3150,2710,3012,4875,
                3540,6127,2581,2642,2573,
                2792,2800,2500,3700,6030,
                5437,2758,3490,2851,2721)
M0 <- 3000  # hypothesized median
# Count days above median
S <- sum(attendance > M0)
n <- length(attendance)
# Two-tailed binomial p-value
p_task1 <- 2 * pbinom(min(S, n-S), n, 0.5)

# --------------------------------------------------------------------------------
# Task 2: Lottery Ticket Sales – Sign test via binomial
# --------------------------------------------------------------------------------
# Given 40 days, 15 have sales < 200
k <- 15; n2 <- 40
# One-tailed p-value
p_task2 <- pbinom(k, n2, 0.5)

# --------------------------------------------------------------------------------
# Task 3: Prison Sentences – Mann-Whitney U test
# --------------------------------------------------------------------------------
males   <- c(8,12,6,14,24,27,32,24,26,19,15,13)
females <- c(7,5,2,3,21,26,30,9,4,17,23,12,11,16)
w3 <- wilcox.test(males, females, alternative = "two.sided", exact = FALSE)
stat_task3 <- w3$statistic; p_task3 <- w3$p.value

# --------------------------------------------------------------------------------
# Task 4: Baseball Wins – Mann-Whitney between leagues
# --------------------------------------------------------------------------------
NL <- c(89,96,88,101,90,91,92,96,108,100,95)
AL <- c(108,86,91,97,100,102,95,104,95,89,88,101)
w4 <- wilcox.test(NL, AL, alternative = "two.sided", exact = FALSE)
stat_task4 <- w4$statistic; p_task4 <- w4$p.value

# --------------------------------------------------------------------------------
# Task 5: Wilcoxon Signed-Rank Critical Values
# --------------------------------------------------------------------------------
# Use statistical tables (Table K) for critical W values. See textbook.
# No R function for direct critical values; student references tables.

# --------------------------------------------------------------------------------
# Task 6: Math Literacy – Kruskal-Wallis test
# --------------------------------------------------------------------------------
scores <- data.frame(
  region = rep(c("West","Europe","East"), each = 5),
  score  = c(527,406,474,381,411,
             520,510,513,548,496,
             523,547,547,391,549)
)
kw6 <- kruskal.test(score ~ region, data = scores)
stat_task6 <- kw6$statistic; p_task6 <- kw6$p.value

# --------------------------------------------------------------------------------
# Task 7: Subway vs Rail – Spearman correlation
# --------------------------------------------------------------------------------
subway <- c(845,494,425,313,108,41)
rail   <- c(39,291,142,103,33,38)
cor7 <- cor.test(subway, rail, method = "spearman")
rho_task7 <- cor7$estimate; p_task7 <- cor7$p.value

# --------------------------------------------------------------------------------
# Task 8: Coupon Collector – Simulation
# --------------------------------------------------------------------------------
sim_coupon <- function(n_sim=10000) {
  replicate(n_sim, {
    seen <- integer(0); count <- 0
    while(length(unique(seen)) < 4) {
      seen <- c(seen, sample(1:4, 1))
      count <- count + 1
    }
    count
  })
}
cc <- sim_coupon(10000)
mean_task8 <- mean(cc)

# --------------------------------------------------------------------------------
# Task 9: Unequal Coupon Collector (Lottery) – Simulation
# --------------------------------------------------------------------------------
probs <- c(b=0.6, i=0.3, g=0.1)
sim_lotto <- function(n_sim=10000) {
  replicate(n_sim, {
    got <- c(b=FALSE, i=FALSE, g=FALSE); count <- 0
    while(!all(got)) {
      draw <- sample(names(probs),1,prob=probs)
      got[draw] <- TRUE; count <- count + 1
    }
    count
  })
}
ll <- sim_lotto(10000)
mean_task9 <- mean(ll)

# --------------------------------------------------------------------------------
# Print Results
print(list(
  Task1 = list(S=S, n=n, p=p_task1),
  Task2 = list(k=k, n=n2, p=p_task2),
  Task3 = list(stat=stat_task3, p=p_task3),
  Task4 = list(stat=stat_task4, p=p_task4),
  Task5 = "See Table K for critical values",
  Task6 = list(stat=stat_task6, p=p_task6),
  Task7 = list(rho=rho_task7, p=p_task7),
  Task8 = mean_task8,
  Task9 = mean_task9
))
