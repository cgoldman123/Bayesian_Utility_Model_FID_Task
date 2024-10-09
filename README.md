# Modeling Choice under Risk

## Setup

Each subject $i$ undergoes $n$ number of trials, in each of which he/she chooses FID from a finite set of options. By choosing a bigger FID the risk of being caught is lowered but the reward is also smaller; a smaller FID, more reward at the cost of higher risk. The utility (reward) is denoted by 

$$r(FID) = r(FID; AD),$$

where $AD$ is a random variable.

Each trial is associated with a color type. Conditioned on the color type $c$ , for each trial AD is independently drawn from $N(\mu^{(c)}, \sigma^{2 (c)})$. 

## Ideal Bayesian Player

This is a sequential decision making problem as the subject should improve estimate of AD to optimize the next choice. Here we model an ideal Bayesian player, who updates belief by the Bayes rule and score the choices accordingly. 

### Unknown mean, known variance

Here we assume the variance of the likelihood $\sigma^{2 (c)}$ is known. 

*(Motive for the assumption : Subjects already played a practice version of the game before the actual experiment, where virtual predators have an identical AD distribution mean to the ones in the actual experiment.)*

The conjugate prior on the mean is also a Gaussian with broad variance $\mu^{(c)} \sim N(\mu^{(c)}_0, \sigma_0^{2 (c)})$.

By observing a sequence of $n$ AD's of type $c$, the posterior is updated to $N(\mu_n^{(c)}, \sigma_n^{2 (c)})$, with

$$1 / \sigma_n^2 = 1 / \sigma_0^2 + n / \sigma^2,$$

$$\mu_n = \sigma_n^2 (\mu_0 / \sigma_0^2 + \sum_i AD_i / \sigma^2).$$

### Bayesian Risk

By independence of AD across trials, the total risk (the expected total negative reward) is a sum of risks over trials. 

Fixing color type $c$, the expected Bayesian reward for trial $i$ is 

$$u_i(FID):= \mathbb{E} r(FID; AD) = \int r(FID; AD) P(AD | \text{previous trials}) d AD,$$

where $P(AD | \text{previous trials})$ is the posterior predictive distribution

$P(AD | …) = \int N(AD | \mu, \sigma^2) P(\mu | …) d \mu = N(AD | \mu_n, \sigma^2 + \sigma_n^2).$

For arbitrary utility $r$, the expectation can be computed with either closed-form or simple Monte Carlo. 

#### Money

The money reward $r_M(FID)$ depends on FID only (a piecewise linear function), if the player escapes; $r_M(FID) = 0$ if unable to escape. 

Hence, 

$$u_M(FID) = \mathbb{E} \mathbf{1}(\text{escape} | FID, AD) r_M(FID) = p(\text{escape} | FID) r_M(FID)$$

#### Pain

Pain, as a negative reward, occurs from electric shock if unable to escape. 

$$u_{P} = -\mathbb{E} \mathbf{1}(\text{not escape} | FID, AD) = -(1 - p(\text{escape} | FID)).$$



### Rational Choice

For given type $c$, the optimal choice is the one that maximizes the expected reward

$$FID_i^{\ast} = \arg \max u_i(FID). $$

## Characterization of Choice Making

Subject $i$ can be characterized by his/her choices relative to the optimal choices made by the ideal Bayesian player. Here in the following we list a few measures. 

### Regret

The gap between the cumulative rewards. 

$$\text{regret} = \sum_i (u_i(FID_i^{\ast}) - u_i(FID_i)).$$

Note that this is defined w.r.t. the **expected** reward from the Bayesian player. 

### Winning Probability

Furthermore, we can quantify the **actual** cumulative reward w.r.t. the distribution of reward from the Bayesian player. 

The cumulative reward gained by playing the optimal strategy $\sum_i r(FID_i^{\ast} | AD_i)​$ is still a random variable. Hence, we can measure the excellence of choice making by the quantile of $\sum_i r(FID_i)​$ in the distribution, namely

$P(\sum_i r(FID_i) > \sum_i r(FID_i^{\ast} | AD_i))$. This number tells the probability that the subject beats an ideal Bayesian player. And this probability is actually a frequentist one.

Again, the quantile can be computed from Monte Carlo. 

### Discrete Choice Model

#### Multinomial Choice Model

Each subject is parameterized by parameter $\beta$, and he/she chooses options independently by probability 

$p_i(FID) = \exp(\beta u_i (FID)) / \sum_{FID} \exp(\beta u_i(FID))$. 

By the independent of choices, $\beta$ can be estimated by maximizing the total log likelihood

$$\hat{\beta} = \arg \max_{\beta} \frac{\beta}{n} \sum_i \log (u_i(FID)) - \log \sum_{FID} \exp(\beta u_i(FID)). $$

The problem is convex and hence $\hat{\beta}$ has unique solution.

The parameter captures the rationality of subject: 

- $\beta \rightarrow +\infty$: rational
- $\beta \rightarrow -\infty$: anti-rational
- $\beta = 0 $: dumb









