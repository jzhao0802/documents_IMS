import pandas as pd
import numpy as np
import scipy.optimize as op
from sklearn.metrics import r2_score
import matplotlib.pyplot as plt
import time

print 'start loading data'
data_raw = pd.read_csv('../../cooked data/data_for_lr_divided.csv')
nrx = pd.read_csv('../../cooked data/boostrix_nrx.csv')
chl_dict = pd.read_csv('../../out data/ref_channel.csv')
decay = pd.read_csv('../../out data/decay_dma.csv')
hcp_type = pd.read_csv('../../cooked data/hcp_type.csv')
promo = pd.read_csv('../../cooked data/hcp_imp.csv')
print 'finish loading data'

doc_posi = hcp_type[(hcp_type['PROMO_FLAG'] == 1) & (hcp_type['BST_FLAG'] == 1)]['CID'].unique()

data = data_raw.copy()



#########
# define function
##########
def Bayesian_LR_adv(X_train, y_train, beta_0, sigma_2, NN):
    K = X_train.shape[1]
    N = X_train.shape[0]
    H = np.append(X_train, np.ones([N ,1]), 1)
    lamb = 1 / sigma_2
    X_prime = np.append(H,   np.sqrt(lamb) * np.eye(K + 1), 0)
    y_prime = np.append(y_train,   np.sqrt(lamb) * beta_0, 0)

    if NN:
        beta = op.nnls(X_prime, y_prime)[0]
    else:
        beta = np.dot(np.linalg.inv( np.dot(X_prime.T, X_prime) ) , np.dot(X_prime.T, y_prime) )
    y_pred = np.dot(H, beta.T)
    return beta, y_pred



#################################
#  compute channel level prior
#################################

prior = pd.read_csv('../../out data/ref_prior.csv')

channels = ['Print', 'Detailing', 'Direct Mail', 'Email',
            'Web', 'Paid Search', 'Digital Display']
beta_prior = np.array([prior[prior['channel'] == item]['Beta'].values[0] for item in channels])

print beta_prior

#array([ 0.01391514,  0.18439073,  0.01191914,  0.01319813,  0.0029694 , 0.00020783,  0.02637767])

beta_prior = [ 0.01391514,  0.98439073,  0.01191914,  0.01319813,  0.0029694 , 0.00020783,  0.02637767]

#################################
#  pull data
#################################
col_raw = ['tt_mkt', 'others', '1', '2', '3', '4', '5', '6', '7']
# col_raw = ['others', '1', '2', '3', '4', '5', '6', '7']
doc_prm = data.CID.unique()
x_promo = data[col_raw].sum(axis = 1)
# plt.hist(x_promo)
HH = x_promo.mean()
print 'hsitorical strength', HH
hist_decay = decay['decay_factor'].mean()
print 'hsitorical decay', hist_decay


use_history = True
beta_other = np.array([0.05, 0.02])
beta_intercept = np.array([0.001])
beta_hist = np.array([0.05])

if use_history:
    data_lr = data.copy()
    data_lr.insert(8, 'history', HH * np.power(hist_decay, data_lr.MTH_IDX.values))
    col_X = col_raw +  ['history']
    X = data_lr[col_X].values
    beta_0 = np.concatenate((beta_other, beta_prior,   beta_hist ,  beta_intercept    ), axis= 0)
else:
    data_lr = data.copy()
    X = data_lr[col_raw].values
    beta_0 = np.concatenate((beta_other, beta_prior ,  beta_intercept    ), axis= 0)

dummy = True
if dummy:
    zz = 1.0 / (data_lr['MTH_IDX'].values + 1)
    data_lr.insert( 9, 'dummy', zz  )
    col_X = col_X + ['dummy']
    X = data_lr[col_X].values
    # X = np.append(X, np.transpose([zz]) , 1 )
    beta_0 = np.insert(beta_0, -1, 0)


y = data_lr['nrx'].values


#beta_0 = np.zeros(beta_0.shape[0])

######################
# train a model
#####################

sigma_2_prime = 1
sigma_2 = 1

data_res = data_lr.copy()
M = y.shape[0]
K =  col_X.__len__() + 1
y_pred_nn = np.zeros(M)
B = np.zeros([M, K])
Z = np.zeros([M, K])

r2 = np.zeros_like(doc_prm)
cc = []
for i, name in enumerate(doc_prm):
    if i % 1000 == 0:
        print i
    df = data_lr[data_lr['CID'] == name]
    ccf = df.shape[0]
    cc.append(ccf)
    # sigma_2 = 1.1 - 0.034 * ccf
    # sigma_2 = 1.5 / ccf

    if ccf > 10:
        beta_nn, y_nn = Bayesian_LR_adv(df[col_X].values, df['nrx'].values,
                                        beta_0, sigma_2_prime, True)
    else:
        beta_nn, y_nn = Bayesian_LR_adv(df[col_X].values, df['nrx'].values,
                                        beta_0, sigma_2, True)

    place = data_lr[data_lr['CID'] == name].index
    y_pred_nn[place] = y_nn
    Z[place] = np.append(df[col_X], np.ones([df[col_X].shape[0], 1]), 1)
    for p in place:
        B[p, :] = beta_nn
    r2[i] = r2_score(df['nrx'].values, y_nn)
print 'shall be zero:', (B * Z).sum() - y_pred_nn.sum()
data_res.insert(0, 'nrx_pred_NN', y_pred_nn)
timestr = time.strftime("%Y%m%d-%H%M%S")
data_res.to_csv('../../temp/predicted_raw_'+ timestr + '.csv', index=False)


#############
# plot figures
#############

curve_true = data_res[['MTH_IDX', 'nrx', 'nrx_pred_NN']].groupby('MTH_IDX').sum().reset_index()
show_y = curve_true['nrx'].values
show_x = curve_true['MTH_IDX'].values
show_w = curve_true['nrx_pred_NN'].values

plt.plot(show_x, show_y, label = 'True DxRx')
plt.plot(show_x, show_w, label = 'Predicted DxRx NN')
# plt.title('R2 square: %4f' % r2_score(show_y, show_w))
plt.legend(loc = 0)
plt.grid()

plt.savefig('../../results/plots/Nation wide NRX predictions using HCP lv Model NN '+ timestr +'.png')

print 'Week Level R2 square:', r2_score(show_y, show_w)

#############
# computer contribution
#############


# 1. contribution by channel

contribution_ratio = [line / line.sum() for line in B * Z]
contribution = np.array( [y[i] * contribution_ratio[i] for i in range(M)] )


# 2. add negative months

nrx_1 = nrx[nrx['CID'].isin(doc_posi)]
nrx_2 = nrx_1[nrx_1['MTH_IDX']< 0]
nrx_3 = nrx_2[['CID', 'value']].groupby('CID').sum().reset_index()


# 3. lift_df

df_lift = pd.DataFrame(columns=col_X + ['intercept'], data=contribution)
df_lift.insert(0, 'CID', data['CID'].values)

temp = pd.DataFrame(columns= df_lift.columns.values)
temp['CID'] = nrx_3['CID'].values
temp['others'] = nrx_3['value'].values

df_lift = pd.concat([df_lift, temp], axis= 0)

print df_lift[col_X + ['intercept']].sum().sum()
# 404438.0

# 4. lift by channel


col_contri = ['contribution: others'] + ['contribution: ' + item for item in channels] \
             + ['contribution: intercept'] + ['contribution: baseline']



contri_res = pd.DataFrame(columns=col_contri)
contri_res['contribution: others'] = df_lift[['tt_mkt', 'others', 'history', 'dummy']].sum(axis=1).values
contri_res['contribution: intercept'] = df_lift['intercept'].values
contri_res['contribution: baseline'] = contri_res['contribution: others'] + contri_res['contribution: intercept']
contri_res[['contribution: ' + item for item in channels]] = df_lift[['1', '2', '3', '4', '5', '6', '7']].values

look = contri_res[['contribution: others'] + ['contribution: ' + item for item in channels] \
                  + ['contribution: intercept']]

print look.sum() / look.sum().sum()

contri_res.insert(0, 'CID', df_lift['CID'].values )
contri_res.to_csv('../../temp/contribution_raw_' + timestr + '.csv', index = False)


contri_over = pd.DataFrame({'NAME': look.sum().index.values, 'lift:': look.sum().values,
                            'ratio':  look.sum().values / look.sum().sum()})
contri_over.to_csv('../../temp/contribution_overview_' + timestr + '.csv', index = False)




##############################
# save coefficient
################################

df_coef_raw = pd.DataFrame(columns=col_X + ['intercept'], data=B)
df_coef_raw.insert(0, 'CID', data['CID'].values)
df_coef = df_coef_raw.groupby('CID').first().reset_index()




df_coef.to_csv('../../temp/coeficient_' + timestr + '.csv', index = False)





###############
# QC results
###############

name = doc_prm[r2.argmin()]

name = doc_prm[np.argmax(cc)]

name = 26665

name = 150483

name = 557214
df = data_lr[data_lr['CID'] == name]
ccf = df.shape[0]
beta_0 = [  5.00000000e-02,   2.00000000e-02,   1.39151400e-02,
         9.84390730e-01,   0,   0,
         2.96940000e-03,   2.07830000e-04,   2.63776700e-02,
         5.00000000e-02,   0.00000000e+00,   1.00000000e-03]

if ccf > 10:
    beta_nn, y_nn = Bayesian_LR_adv(df[col_X].values, df['nrx'].values,
                                    beta_0, sigma_2_prime, True)
else:
    beta_nn, y_nn = Bayesian_LR_adv(df[col_X].values, df['nrx'].values,
                                    beta_0, sigma_2, True)

print y_nn
print df['nrx'].values


#promo[promo['CID'] == name]

zz = np.append(df[col_X].values, np.ones([ df.shape[0] ,1]), 1)
print beta_nn * zz
print r2_score(df['nrx'].values, y_nn)


















# col_coef = ['Others', 'Print', 'Detailing', 'Direct Mail', 'Email',
#         'Web search', 'Paid Search', 'Digital Display', 'intercept']
#
# df = pd.DataFrame(columns=col_coef, data = B)
# df.insert(0, 'CID', data_res['CID'].values)
# df_1 = df.groupby('CID').first().reset_index()
#
# # total_hcp = pd.read_csv('../../raw data/prm_hcp.csv')
# # df_2 = df_1.merge(total_hcp, left_on = 'CID', right_on = 'CID', how = 'left')
# df_1.to_csv('C:\Users\yunlong.wang\Desktop\Nucala final\Part II/results/coeficients_raw.csv', index=False)


#######################
# computer contribution
#######################
# for j, name in enumerate(col_contri):
#     data_res.insert(0, name, contribution[:,j])
#
# data_res.insert(0, 'contribution: baseline', contribution[:,0] + contribution[:,-1])
#
# data_res.to_csv('C:\Users\yunlong.wang\Desktop\Nucala final\Part II/results/contribution_raw.csv', index = False)









