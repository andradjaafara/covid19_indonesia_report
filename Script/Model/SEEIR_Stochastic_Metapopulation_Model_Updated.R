#E and R stage indexed by i andj with
#i for the age group
#j for the progression (not exponential latent and infectious period)

# Time Steps
dt <- user()
time <- step * dt
output(time) <- TRUE

#Number of age classes & number of transmissibility classes
N_reg <- user()

## Core equations for transitions between compartments:
update(S[]) <- S[i] - n_SE[i]
update(E1[]) <- E1[i] + delta_E1[i]
update(E2[]) <- E2[i] + delta_E2[i]
update(I[]) <- I[i] + delta_IMild[i] + delta_ICase[i]
update(IMild[]) <- IMild[i] + delta_IMild[i]
update(ICase[]) <- ICase[i] + delta_ICase[i]
update(R[]) <- R[i] + delta_R[i]

## Individual probabilities of transition:
p_SE[] <- 1 - exp(-lambda[i] * dt) # S to I - age dependent
p_EE <- 1 - exp(-gamma_E * dt) # progression through latent period
p_EI <- 1 - exp(-gamma_E * dt) # progression through latent period
p_IMildR <- 1 - exp(-gamma_IMild * dt) # progression through infectious period
p_ICaseR <- 1 - exp(-gamma_ICase * dt) # progression through infectious period

## Draws from binomial distributions for numbers changing between compartments:
n_SE_matrix[,] <- rbinom(S[i]*m_susceptible[i,j], p_SE[j])
n_SE[] <- sum(n_SE_matrix[i,])# rbinom(S[i], p_SE[i])
n_SE[2] <- 0 # infected outside Java to be kept at 0 all the time
n_EE[] <- rbinom(E1[i], p_EE)
n_EI[] <- rbinom(E2[i], p_EI)
output(n_EI[]) <- TRUE
n_EICase[] <- rbinom(n_EI[i], p_hosp[i])
n_EIMild[] <- n_EI[i] - n_EICase[i]
output(n_EICase[]) <- TRUE
output(n_EIMild[]) <- TRUE
n_IMildR[] <- rbinom(IMild[i], p_IMildR)
n_ICaseR[] <- rbinom(ICase[i], p_ICaseR)

delta_E1[] <- n_SE[i] - n_EE[i]  
delta_E2[] <- n_EE[i] - n_EI[i]  
delta_IMild[] <- n_EIMild[i] - n_IMildR[i]  
delta_ICase[] <- n_EICase[i] - n_ICaseR[i]  
delta_R[] <- n_IMildR[i] + n_ICaseR[i]

#Compute the force of infection

# Interpolation for Mixing Matrix - untransposed
m_susceptible[, ] <- interpolate(tt_matrix_susceptible, mix_mat_set_susceptible, "constant")
dim(m_susceptible) <- c(N_reg, N_reg)
tt_matrix_susceptible[] <- user()
mix_mat_set_susceptible[, ,] <- user()
dim(tt_matrix_susceptible) <- user()
dim(mix_mat_set_susceptible) <- c(length(tt_matrix_susceptible), N_reg, N_reg)

# Interpolation for Mixing Matrix - sick - ICase
m_case[, ] <- interpolate(tt_matrix_case, mix_mat_set_case, "constant")
dim(m_case) <- c(N_reg, N_reg)
tt_matrix_case[] <- user()
mix_mat_set_case[, ,] <- user()
dim(tt_matrix_case) <- user()
dim(mix_mat_set_case) <- c(length(tt_matrix_case), N_reg, N_reg)

# Interpolation for Mixing Matrix - sick - IMild
m_mild[, ] <- interpolate(tt_matrix_mild, mix_mat_set_mild, "constant")
dim(m_mild) <- c(N_reg, N_reg)
tt_matrix_mild[] <- user()
mix_mat_set_mild[, ,] <- user()
dim(tt_matrix_mild) <- user()
dim(mix_mat_set_mild) <- c(length(tt_matrix_mild), N_reg, N_reg)

# Interpolation for rel_trans
rel_trans[] <- interpolate(tt_rel_trans, rel_trans_set, "constant")
dim(rel_trans) <- N_reg
tt_rel_trans[] <- user()
rel_trans_set[,] <- user()
dim(tt_rel_trans) <- user()
dim(rel_trans_set) <- c(length(tt_rel_trans), N_reg)

# number of contacts
dim(contacts_reg) <- N_reg
contacts_reg[] <- user()

# number of infectious contacts
dim(inf_contacts_mild) <- N_reg
dim(inf_contacts_case) <- N_reg
  
# Generating Force of Infection
inf_contacts_mild[] <- IMild[i]
inf_contacts_case[] <- ICase[i]
s_ij[,] <- rbinom(inf_contacts_case[j], m_case[i,j]) + rbinom(inf_contacts_mild[j], m_mild[i,j]) # m[i,j] * inf_contacts[j] # why temp[j] not temp[i]
lambda[] <- pinf[i] * sum(s_ij[i,]) * contacts_reg[i] * rel_trans[i]
lambda[2] <- 0 # lambda outside Java to be kept at 0 all the time
# lambda_avg_matrix[,] <- m_t[i,j] * lambda[j]
# lambda_avg[] <- sum(lambda_avg_matrix[i,])

## Initial states:
initial(S[]) <- S0[i] 
initial(E1[]) <- E0[i]
initial(E2[]) <- E0[i]
initial(I[]) <- IMild0[i]+ICase0[i] 
initial(IMild[]) <- IMild0[i] 
initial(ICase[]) <- ICase0[i] 
initial(R[]) <- R0[i]

##Initial vectors
S0[] <- user()
E0[] <- user()
IMild0[] <- user()
ICase0[] <- user()
R0[] <- user()

##Parameters
gamma_E <- user()
gamma_IMild <- user()
gamma_ICase <- user()
dim(p_hosp) <- N_reg
p_hosp[] <- user()
dim(pinf) <- N_reg
pinf[] <- user()

##Dimensions of the different "vectors" here vectors stand for multi-dimensional arrays
dim(S) <- N_reg
dim(S0) <- N_reg
dim(p_SE) <- N_reg
dim(n_SE) <- N_reg
dim(n_SE_matrix) <- c(N_reg,N_reg)

dim(E1) <- c(N_reg)
dim(E0) <- c(N_reg)
dim(delta_E1) <- c(N_reg)
dim(n_EE) <- c(N_reg)

dim(E2) <- c(N_reg)
dim(delta_E2) <- c(N_reg)
dim(n_EI) <- c(N_reg)

# dim(I0) <- c(N_reg)
dim(I) <- c(N_reg)
# dim(delta_I) <- c(N_reg)
# dim(n_IR) <- c(N_reg)

dim(IMild0) <- c(N_reg)
dim(IMild) <- c(N_reg)
dim(delta_IMild) <- c(N_reg)
dim(n_IMildR) <- c(N_reg)
dim(n_EIMild) <- c(N_reg)

dim(ICase0) <- c(N_reg)
dim(ICase) <- c(N_reg)
dim(delta_ICase) <- c(N_reg)
dim(n_ICaseR) <- c(N_reg)
dim(n_EICase) <- c(N_reg)

dim(R) <- c(N_reg)
dim(R0) <- c(N_reg)
dim(delta_R) <- c(N_reg)

dim(lambda) <- N_reg
# dim(lambda_avg_matrix) <- c(N_reg,N_reg)
# dim(lambda_avg) <- N_reg
dim(s_ij) <- c(N_reg,N_reg)