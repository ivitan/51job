####-----------3、用apriori的关联规则算法挖掘招聘项目中的"公司规模"、“工作经验”、
####“职位月薪”、“最低学历”之间关联关系，支持度大于0.001且置信度大于0.5，并筛选出规则前项---------------

#载入相关程序包
require(arules) 
mydata<-read.csv('/home/vitan/PySpace/re.csv',stringsAsFactors=FALSE,fileEncoding = "utf-8")
head(mydata)
mydata.factor<-data.frame(apply(mydata[,c("name","experience","education","salary","peops")],2,as.factor))

head(mydata.factor)
trans<-as(mydata.factor,"transactions")


#利用apriori算法建模，设置支持度大于0.001且置信度大于0.5
aprioris<-apriori(trans,parameter = list(support=0.001,confidence=0.5))#trans函数是arules中的函数

inspect(trans[1:4])#检查对象
inspect(aprioris)

#职位月薪10001-15000元/月的，一般情况下会同时附带哪些要求？
rules<-subset(aprioris,subset= lhs %in% "salary=3-4万/月")

#给出什么招聘条件下，一般同时要求“最低学历=本科”
rules<-subset(aprioris,subset= rhs %in% "education=本科")
#什么样的招聘条件下，“工作经验=不限”

rules<-subset(aprioris,subset= rhs %in% "experience=不限")

#给出是职位为嵌入式Linux工程师，一般情况下会同时附带什么条件？
#rules<-subset(aprioris,subset= rhs %in% "name=嵌入式Linux工程师")
#对上面提取规则按支持度大小降序排序
rules.sort<-sort(rules,by="lift")

#查看前5条规则，结果参考如下图所示
inspect(rules.sort[1:20])
#itemFrequencyPlot(rules,top=10)
