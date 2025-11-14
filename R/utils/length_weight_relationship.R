waa <- read_csv("../../iml-mackerel/04.0_weight-at-age/csv/waa_2024_base_cv1shrink0.csv")
#laa needs to be updated for 2023-2024......
laa <- read_csv("../../iml-mackerel/12.0_length-at-age/csv/laa_2022_base_cv1shrink0.csv")


lwall= left_join(laa %>% pivot_longer(2:11, names_to="age", values_to="length") ,
 waa %>% pivot_longer(2:11, names_to="age", values_to="weight")  %>%  mutate(weight=weight*1000)) # in grams
ggplot(data=lwall, aes(x=length, y=weight))+geom_point()  
#Length weight -relationship for standard weight
nls_model <- nls(weight ~ a * length^b, data = lwall, start = list(a = 0.00285, b = 3.325))
summary(nls_model)

write_rds(nls_model, "data/utils/nNWAM_Length_weight_nls.RDS")

#a=coefficients(nls_model)[[1]]
#b=coefficients(nls_model)[[2]]


newdata = data.frame(length=0.3:40)
newdata$weight = predict(nls_model,newdata)

ggplot(newdata, aes(x=length, y=weight)) +geom_point()

