# 2 ----------------------------------------------------------------------------
x <- 4:25
y <- seq(-5, 2, length.out = length(x))
y^2
x + y
x / y
sum(x - y)
x[y > 0]
x %*% y

# 3 ----------------------------------------------------------------------------
library(dplyr)
DSjobs <- read.csv2("./data/DSjobs.csv")
DSjobs <- as_tibble(DSjobs)

DSjobs <- select(DSjobs, -starts_with("Time"))
DSjobs <- filter(DSjobs, EmploymentStatus == "Employed full-time")
DSjobs <- mutate(DSjobs, AdjustedCompensation = CompensationAmount*ExchangeRate)
DSjobs <- filter(DSjobs, !is.na(AdjustedCompensation))

DSjobs <- group_by(DSjobs, CurrentJobTitle)
summarize(DSjobs, 
          MeanComp   = mean(AdjustedCompensation),
          MedianComp = median(AdjustedCompensation))

DSjobs %>%
  select(-starts_with("Time")) %>%
  filter(EmploymentStatus == "Employed full-time") %>%
  mutate(AdjustedCompensation = CompensationAmount*ExchangeRate) %>%
  filter(!is.na(AdjustedCompensation)) %>%
  group_by(CurrentJobTitle) %>%
  summarize(MeanComp   = mean(AdjustedCompensation),
            MedianComp = median(AdjustedCompensation))

# 4 ----------------------------------------------------------------------------
library(dplyr)
nba_wage <- read.csv("./data/2017-18_NBA_salary.csv")
nba_wage <- as_tibble(nba_wage)
nba_wage

filter(nba_wage, NBA_Country == "Slovenia")

filter(nba_wage, NBA_Country == "USA", Tm == "GSW")

arrange(nba_wage, desc(Age), desc(Salary))

select(nba_wage, Player, NBA_Country:Tm)

mutate(nba_wage, MPG = MP / G)

group_by(nba_wage, Tm) %>%
  summarise(Team_salary = mean(Salary)) %>%
  arrange(desc(Team_salary))

group_by(nba_wage, NBA_Country) %>%
  summarise(Country_salary = mean(Salary)) %>%
  arrange(desc(Country_salary))

filter(nba_wage, NBA_Country == "USA") %>%
  mutate(Salary_per_MP = Salary / MP) %>% 
  group_by(Tm) %>%
  summarise(Team_salary_per_MP = mean(Salary_per_MP)) %>%
  arrange(desc(Team_salary_per_MP))


# 5 ----------------------------------------------------------------------------
set.seed(0)
A <- matrix(rnorm(16), nrow = 4)
B <- matrix(rnorm(10), nrow = 2)
C <- matrix(rnorm(20), nrow = 5)

mean(A)

var(as.vector(A))

max(c(A,B,C))

A + t(A)

A * t(A)

B %*% C

det(t(B) %*% B)


# 6 ----------------------------------------------------------------------------
my_fun <- function (x) {
  for (i in 1:length(x)) {
    if (x[i] > 0) {
      print("x is greater than 0")
    } else if (x[i] == 0) {
      print("x is zero")
    } else {
      print("x is lower than 0")
    }
  }
}
my_fun(rnorm(30, 3, 2))


# 7 ----------------------------------------------------------------------------
my_fibonacci <- function (n) {
  if (n == 0) {
    return (0)
  } else if (n == 1) {
    return (c(0, 1))
  } else {
    tmp <- c(0, 1)
    for (i in 3:(n + 1)) {
      tmp[i] <- tmp[i - 1] + tmp[i - 2]
    }
    return (tmp)
  }
}
my_fibonacci(10)
