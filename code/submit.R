source('code/include.R', echo = T)

load('data/cln_data.Rd')

mn_train <- train %>%
  summarise_each(funs(mean(., na.rm = T)))

sub_temp <- read_csv('data/IdLookupTable.csv')

submit <- cbind(test, mn_train) %>%
  gather(FeatureName, Location, -ImageId) %>%
  right_join(sub_temp) %>%
  select(-RowId)

write_csv(submit, 'output/sub1.csv')
