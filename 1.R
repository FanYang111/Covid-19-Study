library(opendatatoronto)
library(dplyr)
library(ggplot2)
library(kableExtra)
library("pROC")


package <- show_package("64b54586-6180-4485-83eb-81e8fae3b8fe")
package

# get all resources for this package
resources <- list_package_resources("64b54586-6180-4485-83eb-81e8fae3b8fe")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==1) %>% get_resource()
data

data=data[c(4,5,7,8,11,12,16)]
names(data)=c("Age","Neighbourhood","Source","Classification","Gender","Outcome","Hospitalized")



gender=data.frame(table(data$Gender,data$Hospitalized))
names(gender)=c("Gender","Hospitalization","Counts")
ggplot(data=gender,mapping=aes(x=Gender,y=Counts,fill=Hospitalization))+geom_col()+theme_classic()


confirmed=data.frame(table(data$Classification,data$Hospitalized))
names(confirmed)=c("Classification","Hospitalization","Counts")
ggplot(data=confirmed,mapping=aes(x=Classification,y=Counts,fill=Hospitalization))+geom_col()+theme_classic()

source=table(data$Source,data$Hospitalized)
names(source)=c("Source","Hospitalization","Counts")
d3=round(prop.table(source, margin = 1) * 100,3)
d3 %>%
  kbl(caption = "Table1.  Percentage of Source of Infection in Hospitalization") %>%
  kable_classic(full_width = F, html_font = "Cambria")

data$Hospitalized=ifelse(data$Hospitalized=="No",0,1)

model=glm(Hospitalized~.,data=data,family="binomial")
step(model,direction = "backward")


predicted=predict(model,data,type="response")
rocobj=roc(data$Hospitalized,predicted)
ggroc(rocobj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', round(rocobj$auc,4), ')')) +
  theme_minimal()


model=glm(formula = Hospitalized ~ Age + Source + Classification + 
            Gender + Outcome, family = "binomial", data = data)
knitr::kable(coefficients(summary(model)),caption = "Table2. Summary of Final Model")


neig=table(data$Neighbourhood,data$Hospitalized)
names(neig)=c("Neighbourhood","Hospitalization","Counts")
d3=round(prop.table(neig, margin = 1) * 100,3)
d3 %>%
  kbl(caption = "Table3.  Percentage of Neighbourhood in Hospitalization") %>%
  kable_classic(full_width = F, html_font = "Cambria")





