squ.hyd <- as.data.frame(read.delim("clipboard"))
squ.dry <- as.data.frame(read.delim("clipboard"))
squ.fog <- as.data.frame(read.delim("clipboard"))
names(squ.hyd)


t.test(squ.hyd$TT_Area, squ.dry$TT_Area, paired = TRUE, alternative = "two.sided")

t.test(squ.fog$TT_Area, squ.dry$TT_Area, paired = TRUE, alternative = "two.sided")

t.test(squ.hyd$TT_Area, squ.fog$TT_Area, paired = TRUE, alternative = "two.sided")


names(more80)

more80 <- as.data.frame(read.delim("clipboard"))
less80 <- as.data.frame(read.delim("clipboard"))

t.test(more80$max.VPD.diff, less80$max.VPD.diff, paired = FALSE, alternative = "two.sided")
var(more80$max.VPD.diff)
var(less80$max.VPD.diff)

mean(more80$max.VPD.diff)
mean(less80$max.VPD.diff)