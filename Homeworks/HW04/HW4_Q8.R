df <- read.csv(file = 'diet.csv')
#a) 
# weights before and after
boxplot(df[,c(6,7)], boxfill = NA, border = NA) #invisible boxes - only axes and plot area
boxplot(df[df$diet.type=="A", c(6,7)], xaxt = "n", add = TRUE, boxfill="deeppink", 
        boxwex=0.25, at = 1:ncol(df[,c(6,7)]) - 0.3) #shift these left by -0.15
boxplot(df[df$diet.type=="B", c(6,7)], xaxt = "n", add = TRUE, boxfill="lightblue", 
        boxwex=0.25, at = 1:ncol(df[,c(6,7)])  ) #shift to the right by +0.15
boxplot(df[df$diet.type=="C", c(6,7)], xaxt = "n", add = TRUE, boxfill="orange", 
        boxwex=0.25, at = 1:ncol(df[,c(6,7)]) + 0.3) #shift these left by -0.15
legend(x = "topleft",legend=c("Diet A", "Diet B", "Diet C"), fill = c("deeppink","lightblue","orange"))
# Weight loss differnece in each group
df['difference']<-df$initial.weight-df$final.weight
boxplot(df$difference ~ df$diet.type ,main = "Weight loss in each group boxplot" ,xlab = "Weight loss", ylab="Diet",horizontal = TRUE)

#Height and age in each group
boxplot(df$height ~ df$diet.type ,main = "Diet/Height boxplot" ,xlab = "Height", ylab="Diet",horizontal = TRUE)
boxplot(df$age ~ df$diet.type ,main = "Diet/Age boxplot" ,xlab = "Age", ylab="Diet",horizontal = TRUE)
#####################################################
#c)
df$diet <- factor(df$diet)
result <- aov( difference~diet, data = df)
summary(result)
#####################################################
#d)
pairwise.t.test(df$difference, df$diet, p.adj = "none", pool.sd = FALSE)
pairwise.t.test(df$difference, df$diet, p.adj = "bonf", pool.sd = FALSE)