#==========================================================================================
#	Project: Compare treatment response of Gilenya with Tecfidera

#	Part I: Data Transformation and Preparation

#	Develop time: 08/26/2014 - .

#	Developer: Gui Yaming
#==========================================================================================

data_path<- 'C:\\Documents and Settings\\ymgui\\My Documents\\HEOR\\Gilenya vs Tecfidera response prediction\\data'
#data_path<- 'C:\\Documents and Settings\\ymgui\\Desktop\\Gilenya vs Tecfidera response prediction\\data'
data_file<- 'pred_analytics_file.csv';

setwd(data_path)

raw_data<- read.table(data_file, header=T, sep=',')

# change names of all variables into lower case
names(raw_data)<- tolower(names(raw_data))

# qc raw data
dim(raw_data)
which(is.na(raw_data) == T)

#-----------------------------------------------------------------------
#	Keep only Gilenya (e.g. Fingolimod) and Tecfidera patients
#	create variable treatment
#-----------------------------------------------------------------------
raw_data<- raw_data[which(raw_data$idx_rx %in% c(1,10)),]
dim(raw_data)

# idx_rx=1 means the index medication is Gilenya
treatment<- ifelse(raw_data$idx_rx==1, 1, 0)
table(treatment)
raw_data<- cbind(raw_data , treatment)

#-----------------------------------------------------------------------
#	Make binary flags for all levels of non-binary categorical variables.
#------------------------------------------------------------------------

der_sex_female<- ifelse(raw_data$der_sex=='F' , 1 , 0)
der_sex_male<- ifelse(raw_data$der_sex=='M' , 1 , 0)

non_persistent<- ifelse(raw_data$persistent==0 , 1 , 0)

pat_region_E<- ifelse(raw_data$pat_region=='E' , 1 , 0)
pat_region_MW<- ifelse(raw_data$pat_region=='MW' , 1 , 0)
pat_region_S<- ifelse(raw_data$pat_region=='S' , 1 , 0)
pat_region_W<- ifelse(raw_data$pat_region=='W' , 1 , 0)
sum(pat_region_E + pat_region_MW + pat_region_S + pat_region_W) == length(raw_data$pat_region)

idx_paytype_C<- ifelse(raw_data$idx_paytype=='C' , 1 , 0)
idx_paytype_M<- ifelse(raw_data$idx_paytype=='M' , 1 , 0)
idx_paytype_R<- ifelse(raw_data$idx_paytype=='R' , 1 , 0)
idx_paytype_S<- ifelse(raw_data$idx_paytype=='S' , 1 , 0)
idx_paytype_U<- ifelse(raw_data$idx_paytype=='U' , 1 , 0)
sum(idx_paytype_C+ idx_paytype_M+ idx_paytype_R+ idx_paytype_S+ idx_paytype_U) == length(raw_data$idx_paytype)

idx_prodtype_D<- ifelse(raw_data$idx_prodtype=='D' , 1 , 0)
idx_prodtype_H<- ifelse(raw_data$idx_prodtype=='H' , 1 , 0)
idx_prodtype_I<- ifelse(raw_data$idx_prodtype=='I' , 1 , 0)
idx_prodtype_P<- ifelse(raw_data$idx_prodtype=='P' , 1 , 0)
idx_prodtype_S<- ifelse(raw_data$idx_prodtype=='S' , 1 , 0)
idx_prodtype_U<- ifelse(raw_data$idx_prodtype %in% c('U','R') , 1 , 0)
sum(idx_prodtype_D+ idx_prodtype_H+ idx_prodtype_I+ idx_prodtype_P+ idx_prodtype_S+ idx_prodtype_U) == length(raw_data$idx_prodtype)

# '01' = General practice/Family practice, '02' = Internal medicine, '03' = Neurology, '04' = Other, '05' = unknown"
idx_spec_01<- ifelse(raw_data$idx_spec==1 , 1 , 0)
idx_spec_02<- ifelse(raw_data$idx_spec==2 , 1 , 0)
idx_spec_03<- ifelse(raw_data$idx_spec==3 , 1 , 0)
idx_spec_04<- ifelse(raw_data$idx_spec==4 , 1 , 0)
idx_spec_05<- ifelse(raw_data$idx_spec==5 , 1 , 0)
sum(idx_spec_01 + idx_spec_02 + idx_spec_03 + idx_spec_04 + idx_spec_05) == length(raw_data$idx_spec)

new_create_binary_covar<- cbind(#non_persistent,
	der_sex_female,
	der_sex_male,
	pat_region_E,
	pat_region_MW,
	pat_region_S,
	pat_region_W,
	idx_paytype_C,
	idx_paytype_M,
	idx_paytype_R,
	idx_paytype_S,
	idx_paytype_U,
	idx_prodtype_D,
	idx_prodtype_H,
	idx_prodtype_I,
	idx_prodtype_P,
	idx_prodtype_S,
	idx_prodtype_U,
	idx_spec_01,
	idx_spec_02,
	idx_spec_03,
	idx_spec_04,
	idx_spec_05
)

#-----------------------------------------------------------------------
#	Create quartiles / bins for all continuous and count variables.
#	Make sure observations with the same value are in the same bin / quartile
#	For count variables make sure after quartile each cell have no less than 15% patients
#------------------------------------------------------------------------
quartiles<- function(x){
	eval(parse(text = paste('re<- raw_data$' , x , sep='')))
	q<- quantile(re , c(1,2,3)/4)
	x4<- ifelse(re >= q[3] , 1 , 0)
	x3<- ifelse(re >= q[2] & re < q[3] , 1 , 0)
	x2<- ifelse(re >= q[1] & re < q[2] , 1 , 0)
	x1<- ifelse(re < q[1] , 1 , 0)
	result<- data.frame(x1 , x2 , x3 , x4)
	colnames(result)<- paste(x , 1:4 , sep='_')
	range_of_bin<- c(paste('<', q[1], sep=''), paste('[', q[1], ',', q[2], ')', sep=''), 
		paste('[', q[2], ',', q[3], ')', sep=''), paste('>=', q[3], sep=''))
	return(result)
}

pre_non_ms_total_allowed_quartiles <- quartiles('pre_non_ms_total_allowed')
pre_ms_total_allowed_quartiles <- quartiles('pre_ms_total_allowed')
pre_non_ms_pharmacy_allowed_quartiles <- quartiles('pre_non_ms_pharmacy_allowed')
pre_ms_pharmacy_allowed_quartiles <- quartiles('pre_ms_pharmacy_allowed')
pre_non_ms_medical_allowed_quartiles <- quartiles('pre_non_ms_medical_allowed')
pre_ms_medical_allowed_quartiles <- quartiles('pre_ms_medical_allowed')

num_pre_meds_quartiles<- quartiles('num_pre_meds')
num_pre_op_dx_quartiles<- quartiles('num_pre_op_dx')

age_1<- ifelse(raw_data$age < 35 , 1 , 0)
age_2<- ifelse(raw_data$age >= 35 & raw_data$age <= 44 , 1 , 0)
age_3<- ifelse(raw_data$age >= 45 & raw_data$age <= 54 , 1 , 0)
age_4<- ifelse(raw_data$age >= 55 , 1 , 0)
age_quartiles<- data.frame(age_1, age_2, age_3, age_4)
sum(age_quartiles[,1]) + sum(age_quartiles[,2]) + sum(age_quartiles[,3]) + sum(age_quartiles[,4])

pchrlson_1<- ifelse(raw_data$pchrlson==0 , 1 , 0)
pchrlson_2<- ifelse(raw_data$pchrlson==1 , 1 , 0)
pchrlson_3<- ifelse(raw_data$pchrlson >= 2 , 1 , 0)
pchrlson_quartiles<- data.frame(pchrlson_1 , pchrlson_2 , pchrlson_3)

num_pre_mri_any_1<- ifelse(raw_data$num_pre_mri_any==0, 1, 0)
num_pre_mri_any_2<- ifelse(raw_data$num_pre_mri_any==1, 1, 0)
num_pre_mri_any_3<- ifelse(raw_data$num_pre_mri_any==2, 1, 0)
num_pre_mri_any_4<- ifelse(raw_data$num_pre_mri_any>=3, 1, 0)
num_pre_mri_any_quartiles<- data.frame(num_pre_mri_any_1, num_pre_mri_any_2, num_pre_mri_any_3, num_pre_mri_any_4)

num_pre_cort_oral_1<- ifelse(raw_data$num_pre_cort_oral==0, 1, 0)
num_pre_cort_oral_2<- ifelse(raw_data$num_pre_cort_oral==1, 1, 0)
num_pre_cort_oral_3<- ifelse(raw_data$num_pre_cort_oral>=2, 1, 0)
num_pre_cort_oral_quartiles<- data.frame(num_pre_cort_oral_1, num_pre_cort_oral_2, num_pre_cort_oral_3)

num_pre_cort_iv_1<- ifelse(raw_data$num_pre_cort_iv==0, 1, 0)
num_pre_cort_iv_2<- ifelse(raw_data$num_pre_cort_iv %in% 1:2, 1, 0)
num_pre_cort_iv_3<- ifelse(raw_data$num_pre_cort_iv>=3, 1, 0)
num_pre_cort_iv_quartiles<- data.frame(num_pre_cort_iv_1, num_pre_cort_iv_2, num_pre_cort_iv_3)

num_pre_relapses_1<- ifelse(raw_data$num_pre_relapses==0, 1 , 0)
num_pre_relapses_2<- ifelse(raw_data$num_pre_relapses==1, 1 , 0)
num_pre_relapses_3<- ifelse(raw_data$num_pre_relapses>=2, 1 , 0)
num_pre_relapses_quartiles<- data.frame(num_pre_relapses_1, num_pre_relapses_2, num_pre_relapses_3)

#persist_days_1<- ifelse(raw_data$persist_days<180, 1, 0)
#persist_days_2<- ifelse(raw_data$persist_days>=180, 1, 0)
#persist_days_quartiles<- data.frame(persist_days_1 , persist_days_2)

new_create_quartile_covar<- cbind(
	pre_non_ms_total_allowed_quartiles,
	pre_ms_total_allowed_quartiles,
	pre_non_ms_pharmacy_allowed_quartiles,
	pre_ms_pharmacy_allowed_quartiles,
	pre_non_ms_medical_allowed_quartiles,
	pre_ms_medical_allowed_quartiles,
	num_pre_meds_quartiles,
	num_pre_op_dx_quartiles,
	age_quartiles,
	pchrlson_quartiles,
	num_pre_mri_any_quartiles,
	num_pre_cort_oral_quartiles,
	num_pre_cort_iv_quartiles,
	num_pre_relapses_quartiles
	#persist_days_quartiles
)
dim(new_create_quartile_covar)

#-----------------------------------------------------------------------
#	Create attribution lookup table about DetailVar/SummaryVar for all variables
#-----------------------------------------------------------------------

total_covar_list<-c('persistent',
	'persist_days',
	'pre_rx_any',
	paste('pre_rx', 1:11, sep=''),
	'pre_ampyra1',
	'pre_mri_any',
	'pre_90_mri_any',
	'pre_90_cort_any',
	'pre_cort_oral',
	'pre_cort_iv',
	paste('pre_comor', 1:33, sep=''),
	'pre_relapse',
	'pre_relapse_90',
	'pre_ip_relapse',
	colnames(new_create_binary_covar),
	colnames(new_create_quartile_covar)
)

#write.csv(total_covar_list , 'variable attribution.csv', row.names=F, quote=F)

# codify the attribute of each variable about whether it is summary or detail variable
covar_attr<- read.table('variable attribution.csv', header=T, sep=',')
colnames(covar_attr)<- tolower(colnames(covar_attr))
colnames(covar_attr)
total_covar_list %in% covar_attr$variable
total_covar_list<- total_covar_list[match(intersect(covar_attr$variable, total_covar_list) , total_covar_list)] # keep the same order

summary_covar<- as.character(covar_attr$variable[which(covar_attr$summary_variable == 1)])
detail_covar<- as.character(covar_attr$variable[which(covar_attr$detail_variable == 1)])
length(summary_covar) ; length(detail_covar)

#-----------------------------------------------------------------------
#	Create two-way interaction terms involving the treatment (i.e. Gilenya * covariate)
#	Create training and test sample group
#-----------------------------------------------------------------------
dim(raw_data)
raw_data<- cbind(raw_data, new_create_binary_covar, new_create_quartile_covar)
dim(raw_data)

# create interaction variable between treatment and other co-variables
interaction_term<- apply(raw_data[, match(total_covar_list , names(raw_data))] , 2 , function(x){x * raw_data$treatment})
colnames(interaction_term)<- paste(total_covar_list , '_interact' , sep='')
raw_data<- cbind(raw_data , interaction_term)
dim(raw_data)

# randomly sample 80% of patients as training group, and another 20% as test group
training<- numeric(length=nrow(raw_data))
case<- which(raw_data$post_relapse_persist==1)
control<- which(raw_data$post_relapse_persist==0)
set.seed(100)
index_case<- sample(case , round(length(case)*0.8))
set.seed(100)
index_control<- sample(control , round(length(control)*0.8))
training[c(index_case , index_control)]<- 1
raw_data<- cbind(raw_data , training)

table(training , raw_data$post_relapse_persist)

# drop those covariates with standard deviation equal 0
var_list<- c(total_covar_list , colnames(interaction_term))
constant_covar<- apply(raw_data[, match(var_list , names(raw_data))] , 2 , sd)
constant_covar<- names(constant_covar[constant_covar==0])

apply(raw_data[, match(var_list , names(raw_data))] , 2 , function(x) table(x))


#-----------------------------------------------------------------------
#	Descriptive statistics
#-----------------------------------------------------------------------

# distribution of each variable
var_list<- c(total_covar_list , colnames(interaction_term), 'treatment', 'post_relapse_persist')

OUT<- file('variable_distribution.txt' , 'w')
for(i in var_list){
	eval(parse(text = paste('x<- raw_data$' , i , sep='')))
	re<- table(x)
	nm<- names(re)
	pc<- sprintf('%.2f' , re/length(x)*100)
	writeLines(paste(i, 'Frequency', 'Percent' , sep='\t\t') , OUT)
	writeLines(paste(rep('-' , 100) , collapse='') , OUT)
	for(j in 1:length(re)){
		writeLines(paste(nm[j] , re[j] , pc[j] , sep='\t\t') , OUT)
	}
	writeLines('' , OUT)
}
close(OUT)


# covariance matrix of co-variables
var_list<- setdiff(c(total_covar_list , colnames(interaction_term)) , constant_covar)

clean_model_data<- raw_data[, match(var_list , names(raw_data))]
covariance_matrix<- cor(clean_model_data)
write.csv(covariance_matrix , 'covariance_matrix.csv', row.names=T, quote=F)
pdf("covariance_matrix.pdf", height=6, width=8, pointsize=12)
	heatmap(covariance_matrix)
dev.off()

high_correlation<- function(x){
	for(i in 2:length(var_list)-1){
		for(j in (i+1):length(var_list)){
			if(covariance_matrix[i,j] > x){
				print(paste(var_list[i] , var_list[j] , sep=' : '))
			}
		}
	}
}


# Test the association between response variable(post_relapse_persist) and each co-variable
var_list<- setdiff(c(total_covar_list , colnames(interaction_term), 'treatment') , constant_covar)

correlation<- apply(raw_data[, match(var_list , names(raw_data))] , 2 , function(x){
		re<- cor.test(x , raw_data$post_relapse_persist)
		return(c(re$estimate , re$p.value))
})
correlation<- t(correlation)
write.csv(correlation , 'association_with_response.csv', row.names=T, sep='\t', quote=F)


# Relative risk of post-relapse(variable post_relapse_persist) for Gilenya .vs. Tecfidera by covariate.
#Note I: the response variable used to calculate odds ratio is post_relapse_persist
#Note II: odds ratio and p-value is calculated by Fisher-exact test

OR_by_covar<- function(x){
	variable_list<- c(x)
	result<- NULL
	for(i in 1:length(variable_list)){
		eval(parse(text = paste('var_data<- raw_data$' , variable_list[i] , sep='')))
		index<- which(var_data==1)
		Gilenya_relapse<- length(which(raw_data$post_relapse_persist[index]==1 & raw_data$treatment[index]==1))
		Gilenya_not_relapse<- length(which(raw_data$post_relapse_persist[index]==0 & raw_data$treatment[index]==1))
		Tecfidera_relapse<- length(which(raw_data$post_relapse_persist[index]==1 & raw_data$treatment[index]==0))
		Tecfidera_not_relapse<- length(which(raw_data$post_relapse_persist[index]==0 & raw_data$treatment[index]==0))

		#RR<- round((n1/(n1+n2)) / (n3/(n3+n4)) , 2)
		#OR<- round((n1/n2) / (n3/n4) , 2)
		Gilenya_relapse_rate<- Gilenya_relapse/(Gilenya_relapse+Gilenya_not_relapse)
		Tecfidera_relapse_rate<- Tecfidera_relapse/(Tecfidera_relapse+Tecfidera_not_relapse)

		contigency_table<- matrix(c(Gilenya_relapse, Tecfidera_relapse, Gilenya_not_relapse, Tecfidera_not_relapse), nc=2, byrow=T)
		association_test<- fisher.test(contigency_table , alternative = "two.sided")
		p_value<- association_test$p.value
		odds_ratio<- association_test$estimate
					
		result<- rbind(result , c(variable_list[i], Gilenya_relapse, Gilenya_not_relapse, Gilenya_relapse_rate, 
						Tecfidera_relapse, Tecfidera_not_relapse, Tecfidera_relapse_rate, odds_ratio, p_value))
	}
	colnames(result)<- c('Covariate', 'Gilenya_relapse', 'Gilenya_not_relapse', 'Gilenya_relapse_rate', 
			'Tecfidera_relapse', 'Tecfidera_not_relapse', 'Tecfidera_relapse_rate', 'odds_ratio', 'p_value')
	result<- as.data.frame(result)
	return(result)
}

var_list<- setdiff(c('non_persistent' , total_covar_list), c('persist_days'))
OR_result<- OR_by_covar(var_list)
write.csv(OR_result, "Odds Ratio by Covariates.csv", row.names=F, quote=F)

OR_result[match(covar_attr$variable , OR_result$variable),]
write.csv(OR_result[match(covar_attr$variable , OR_result$variable),] , 'result.csv', row.names=F, quote=F)


# Relative risk of post-relapse(variable post_relapse_persist) for each co-variable.
#Note I: the response variable used to calculate odds ratio is post_relapse_persist
#Note II: odds ratio and p-value is calculated by Fisher-exact test
#Note III: when calculate the odds ratio for each covariate, the case means covariate=1 while control means covariate=0

OR_of_covar<- function(x){
	variable_list<- c(x)
	result<- NULL
	for(i in 1:length(variable_list)){
		eval(parse(text = paste('var_data<- raw_data$' , variable_list[i] , sep='')))
		Gilenya_relapse<- length(which(raw_data$post_relapse_persist==1 & var_data==1))
		Gilenya_not_relapse<- length(which(raw_data$post_relapse_persist==0 & var_data==1))
		Tecfidera_relapse<- length(which(raw_data$post_relapse_persist==1 & var_data==0))
		Tecfidera_not_relapse<- length(which(raw_data$post_relapse_persist==0 & var_data==0))

		Gilenya_relapse_rate<- round(Gilenya_relapse/(Gilenya_relapse+Gilenya_not_relapse) , 4)
		Tecfidera_relapse_rate<- round(Tecfidera_relapse/(Tecfidera_relapse+Tecfidera_not_relapse) , 4)

		contigency_table<- matrix(c(Gilenya_relapse, Tecfidera_relapse, Gilenya_not_relapse, Tecfidera_not_relapse), nc=2, byrow=T)
		association_test<- fisher.test(contigency_table , alternative = "two.sided")
		p_value<- round(association_test$p.value , 2)
		odds_ratio<- round(association_test$estimate , 2)
					
		result<- rbind(result , c(variable_list[i], Gilenya_relapse, Gilenya_not_relapse, Gilenya_relapse_rate, 
						Tecfidera_relapse, Tecfidera_not_relapse, Tecfidera_relapse_rate, odds_ratio, p_value))
	}
	colnames(result)<- c('variable', 'case_relapse', 'case_not_relapse', 'case_relapse_rate', 
			'control_relapse', 'control_not_relapse', 'control_relapse_rate', 'odds_ratio', 'p_value')
	result<- as.data.frame(result)
	return(result)
}

var_list<- c('treatment', total_covar_list , colnames(interaction_term))
var_list<- setdiff(var_list , c('persist_days', 'persist_days_interact'))
OR_result<- OR_of_covar(var_list)
write.csv(OR_result, "Odds Ratio of Covariates.csv", row.names=F, quote=F)

OR_result[match(covar_attr$variable , OR_result$variable),]
write.csv(OR_result[match(covar_attr$variable , OR_result$variable),] , 'result.csv', row.names=F, quote=F)

re<- OR_result[match(paste(covar_attr$variable, '_interact', sep='') , OR_result$variable),]
write.csv(re , 'result.csv', row.names=F, quote=F)

# QC
table(raw_data$post_relapse_persist , )
