source("../cleaning_packages.R")


#NHANES Continuous=============================================================






#NHANES III====================================================================


#=====update log file==========================================================

#write update message
message='
Initiate log.
'

#update log
update_log(file='log_mortality.txt',
           author='Peter T. Tanksley',
           message = message)
