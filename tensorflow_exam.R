library(keras)
library(tfruns)

#install.packages("keras")
#install.packages("tfestimators")
training_run("mnist_mlp.R")
training_run("mnist.R")
tensorboard()

#tensorboard(c("logs/run_a", "logs/run_b"))
view_run()
#view_run("runs/2018-05-25T10-06-48Z")
view_run("runs/2018-06-11T13-45-21Z")


View(ls_runs())
compare_runs(ls_runs(eval_acc > 0.9570, order = eval_acc))





library(tfdatasets)


dataset <- text_line_dataset("mtcars.csv", record_spec = mtcars_spec) %>% 
  dataset_prepare(x = c(mpg, disp), y = cyl) %>% 
  dataset_shuffle(5000) %>% 
  dataset_batch(128) %>% 
  dataset_repeat() # repeat infinitely

batch <- next_batch(dataset)

steps <- 200
for (i in 1:steps) {
  # use batch$x and batch$y tensors
}







# create specification for parsing records from an example file
iris_spec <- csv_record_spec("iris.csv")

# read the datset
dataset <- text_line_dataset("iris.csv", record_spec = iris_spec) 

# take a glimpse at the dataset
str(dataset)