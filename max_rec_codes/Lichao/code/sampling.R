sampling <- function(data, prop.tr, prop.val, prop.test) {
    require(caret)
    if (!(0<=prop.tr<=1 and 0<=prop.tr<=1 and 0<=prop.tr<=1 and sum(prop.tr+prop.val+prop.test)==1)) stop("Please confirm the proportion!!")
    pos_idx <- which(data$response==1)
    neg_idx <- which(data$response==0)
    get_idx <- function(label_idx, prop.tr, prop.val, prop.test){
         
        tr_idx <- sample(label_idx, prop.tr*length(label_idx), replace=FALSE)
        val_idx <- sample(setdiff(label_idx, tr_idx), prop.val*length(label_idx), replace=FALSE)
        test_idx <- setdiff(label_idx, c(tr_idx, val_idx))
        return(list(tr_idx=tr_idx, val_idx=val_idx, test_idx=test_idx))
    }
    pos_idx_list <- get_idx(label_idx=pos_idx, prop.tr, prop.val, prop.test)
    neg_idx_list <- get_idx(label_idx=neg_idx, prop.tr, prop.val, prop.test)
    
    flag <- numeric(nrow(data))
    flag[c(pos_idx_list[[1]], neg_idx_list[[1]])]=1
    flag[c(pos_idx_list[[2]], neg_idx_list[[2]])]=2
    
  
  
  
  
  resp_pos <- subset(data.frame(data), response == TRUE)
  resp_neg <- subset(data.frame(data), response == FALSE)
#   cnt_tru <- subset(data.frame(data), treatment == 0 & response == TRUE)
#   cnt_fal <- subset(data.frame(data), treatment == 0 & response == FALSE)
  
  #set.seed(12345)
    
    resp_pos_training_flag <- rep(0, nrow(resp_pos))
    resp_pos_flag <- sample(1:nrow(resp_pos), portion * nrow(resp_pos), replace = FALSE)
    resp_pos_training_flag[resp_pos_flag] <- 1
    # trt_tru_sample <- trt_tru[trt_tru_flag, ]
    
    resp_neg_training_flag <- rep(0, nrow(resp_neg))
    resp_neg_flag <- sample(1:nrow(resp_neg), portion * nrow(resp_neg), replace = FALSE)
    resp_neg_training_flag[resp_neg_flag] <- 1 
    # trt_fal_sample <- trt_fal[trt_fal_flag, ]
    resp_pos <- setdiff(resp_pos, )
    resp_pos_valid_flag <- rep(0, nrow(resp_pos))
    resp_pos_idx <- sample(1:nrow(resp_pos))
#     cnt_tru_training_flag <- rep(0, nrow(cnt_tru))
#     cnt_tru_flag <- sample(1:nrow(cnt_tru), round(portion * nrow(cnt_tru)), replace = FALSE)
#     cnt_tru_training_flag[cnt_tru_flag] <- 1
#     # cnt_tru_sample <- cnt_tru[cnt_tru_flag, ]
#     
#     cnt_fal_training_flag <- rep(0, nrow(cnt_fal))
#     cnt_fal_flag <- sample(1:nrow(cnt_fal), round(portion * nrow(cnt_fal)), replace = FALSE)
#     cnt_fal_training_flag[cnt_fal_flag] <- 1
    # cnt_fal_sample <- cnt_fal[cnt_fal_flag, ]
    
    univ <- rbind(resp_pos, resp_neg)
    training_flag <- c(resp_pos_training_flag, resp_neg_training_flag)

    return(data.frame(univ, training_flag = training_flag))
  

}