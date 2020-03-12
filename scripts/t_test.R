squ.hyd <- as.data.frame(read.delim("clipboard"))
squ.dry <- as.data.frame(read.delim("clipboard"))
squ.fog <- as.data.frame(read.delim("clipboard"))
names(squ.hyd)


t.test(squ.hyd$TT_Area, squ.dry$TT_Area, paired = TRUE, alternative = "two.sided")

t.test(squ.fog$TT_Area, squ.dry$TT_Area, paired = TRUE, alternative = "two.sided")

t.test(squ.hyd$TT_Area, squ.fog$TT_Area, paired = TRUE, alternative = "two.sided")


names(less80)

more80 <- as.data.frame(read.delim("clipboard"))
less80 <- as.data.frame(read.delim("clipboard"))

t.test(more80$seconds, less80$seconds, paired = FALSE, alternative = "two.sided")
var(more80$max.VPD.diff)
var(less80$max.VPD.diff)

mean(more80$seconds)
mean(less80$seconds)

var.test(more80$seconds,less80$seconds)#F test

bin<- as.data.frame(read.delim("clipboard"))
mean(bin$seconds)
var(bin$seconds)
