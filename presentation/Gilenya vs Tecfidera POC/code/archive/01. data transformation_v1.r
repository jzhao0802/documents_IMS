#==========================================================================================
#	Project: Compare treatment response of Gilenya with Tecfidera

#	Part I: Data Transformation and Preparation

#	Develop time: 08/26/2014 - .
#==========================================================================================

data_path<- 'C:\\Documents and Settings\\ymgui\\My Documents\\HEOR\\Gilenya vs Tecfidera response prediction\\data';
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
#-----------------------------------------------------------------------
raw_data<- raw_data[which(raw_data$idx_rx %in% c(1,10)),]
dim(raw_data)

#-----------------------------------------------------------------------
#	Make binary flags for all levels of non-binary categorical variables.
#------------------------------------------------------------------------

# idx_rx=1 means the index medication is Gilenya
treatment<- ifelse(raw_data$idx_rx==1, 1, 0)
table(treatment)

der_sex_female<- ifelse(raw_data$der_sex=='F' , 1 , 0)
der_sex_male<- ifelse(raw_data$der_sex=='M' , 1 , 0)

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

new_create_binary_var<- cbind(treatment,
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
	sub_range<- c(paste('<', q[1], sep=''), paste('[', q[1], ',', q[2], ')', sep=''), 
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

new_create_quartile_var<- cbind(
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
dim(new_create_quartile_var)

#-----------------------------------------------------------------------
#	Create attribution lookup table about DetailVar/SummaryVar for all variables
#	It still needs to find a more efficient and easier way to mark the attribution of variable
#-----------------------------------------------------------------------
response_var<- c('post_relapse', 'post_relapse_persist')

new_create_var<- c(colnames(new_create_binary_var) , colnames(new_create_quartile_var))

new_create_var_detailvar<- setdiff(new_create_var , c(colnames(pre_non_ms_pharmacy_allowed_quartiles),
	colnames(pre_ms_pharmacy_allowed_quartiles),
	colnames(pre_non_ms_medical_allowed_quartiles),
	colnames(pre_ms_medical_allowed_quartiles)))
	
new_create_var_summaryvar<- setdiff(new_create_var , c(colnames(pre_non_ms_total_allowed_quartiles),
	colnames(pre_ms_total_allowed_quartiles)))

detailvar<- c(paste('pre_rx', 1:11, sep=''),
	'pre_ampyra1',
	'pre_90_mri_any',
	'pre_90_cort_any',
	'pre_cort_oral','pre_cort_iv',
	paste('pre_comor', 1:33, sep=''),
	'pre_relapse_90',
	'pre_ip_relapse',
	'persistent',
	new_create_var_detailvar
)

summaryvar<- c(
	'pre_rx_any',
	'pre_ampyra1',
	'pre_mri_any',
	'pre_90_cort_any',
	'pre_cort_oral',
	'pre_cort_iv',
	paste('pre_comor', 1:33, sep=''),
	'pre_relapse',
	'persistent',
	new_create_var_summaryvar
)

total_covar_list<-c(paste('pre_rx', 1:11, sep=''),
	'pre_rx_any',
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
	'persistent',
	'persist_days',
	new_create_var
)

#-----------------------------------------------------------------------
#	Combine the raw data with newly created binary variables
#	Create training and test sample group
#	Create two-way interaction terms involving the treatment (i.e. Gilenya * covariate). 
#-----------------------------------------------------------------------
dim(raw_data)
raw_data<- cbind(raw_data, new_create_binary_var, new_create_quartile_var)
dim(raw_data)

# create interaction variable between treatment and other co-variables
interaction_term<- apply(raw_data[, match(total_covar_list , names(raw_data))] , 2 , function(x){x * raw_data$treatment})
colnames(interaction_term)<- paste(total_covar_list , '_interact' , sep='')

model_data<- cbind(raw_data , interaction_term)
total_covar_list<- setdiff(c(total_covar_list , colnames(interaction_term)) , 'treatment_interact')

# randomly sample 80% of patients as training group, and another 20% as test group
training<- numeric(length=nrow(raw_data))
case<- which(raw_data$post_relapse_persist==1)
control<- which(raw_data$post_relapse_persist==0)
set.seed(100)
index_case<- sample(case , round(length(case)*0.8))
set.seed(100)
index_control<- sample(control , round(length(control)*0.8))
training[c(index_case , index_control)]<- 1
table(training , raw_data$post_relapse_persist)
model_data<- cbind(model_data , training)

# drop out those variables with standard deviation equal 0
constant_var<- apply(model_data[, match(total_covar_list , names(model_data))] , 2 , sd)
constant_var<- names(constant_var[constant_var==0])
total_covar_list<- setdiff(total_covar_list , constant_var)

# QC new created data set: model_data
dim(model_data)
names(model_data)
model_data[1:5,]

#-----------------------------------------------------------------------
#	Descriptive statistics
#-----------------------------------------------------------------------

# distribution of each variable
OUT<- file('variable_distribution.txt' , 'w')
for(i in total_covar_list){
	eval(parse(text = paste('x<- model_data$' , i , sep='')))
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


# covariance_matrix of co-variables
clean_model_data<- model_data[, match(total_covar_list , names(model_data))]
covariance_matrix<- cor(clean_model_data)
write.table(covariance_matrix , 'covariance_matrix.txt', col.names=T, row.names=T, sep='\t', quote=F)
pdf("covariance_matrix.pdf", height=6, width=8, pointsize=12)
	heatmap(covariance_matrix)
dev.off()

high_correlation<- function(x){
	for(i in 2:length(total_covar_list)-1){
		for(j in (i+1):length(total_covar_list)){
			if(covariance_matrix[i,j] > x){
				print(paste(total_covar_list[i] , total_covar_list[j] , sep=' : '))
			}
		}
	}
}


# Test the Correlation between response variable(post_relapse) and each co-variable
correlation<- apply(model_data[, match(total_covar_list , names(model_data))] , 2 , function(x){
		re<- cor.test(x , model_data$post_relapse)
		return(c(re$estimate , re$p.value))
})
correlation<- t(correlation)
write.table(correlation , 'correlation_with_response.txt', col.names=F, row.names=T, sep='\t', quote=F)


# Relative risk of post-relapse for Gilenya compared to Tecfidera by each co-variable.
RR_division<- function(x){
	var_list<- c(x)
	result<- character(length(var_list))
	for(i in 1:length(var_list)){
		eval(parse(text = paste('var_data<- model_data$' , var_list[i] , sep='')))
		index<- which(var_data==1)
		n1<- length(which(model_data$post_relapse[index]==1 & model_data$treatment[index]==1))
		n2<- length(which(model_data$post_relapse[index]==0 & model_data$treatment[index]==1))
		n3<- length(which(model_data$post_relapse[index]==1 & model_data$treatment[index]==0))
		n4<- length(which(model_data$post_relapse[index]==0 & model_data$treatment[index]==0))

		RR<- sprintf('%.2f' , (n1/(n1+n2)) / (n3/(n3+n4)))
		Gilenya_relapse_rate<- sprintf('%.4f' , n1/(n1+n2))
		Tecfidera_relapse_rate<- sprintf('%.4f' , n3/(n3+n4))

		contigency_table<- matrix(c(n1, n3, n2, n4), nc=2, byrow=T)
		association_test<- fisher.test(contigency_table , alternative = "two.sided")
		p_value<- sprintf('%.2f' , association_test$p.value)
					
		result[i]<- paste(var_list[i], n1, n2, Gilenya_relapse_rate, n3, n4, Tecfidera_relapse_rate, RR, p_value, collapse='\t')
	}
	return(result)
}

write.table(RR_division(total_covar_list), "Relative Risk by co-variables.txt", col.names=F, row.names=F, quote=F)
