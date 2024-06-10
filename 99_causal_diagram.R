# 99_causal_diagram.R
# causal diagram to explain confounding in original design
# June 2024
library(diagram)

# text
labels = c('Pre 2015','Post 2015','Random funding\n(expsosure)','Publication/\nCitation counts\n(outcome)')
n.labels = length(labels)

frame = read.table(sep='\t', stringsAsFactors=F, skip=0, header=T, text='
i	x	y	box.col	box.type	box.prop	box.size
1	0.1	0.5	grey77	square	0.48	0.08
2	0.4	0.5	grey77	square	0.48	0.08
3	0.5	0.1	grey77	square	0.48	0.13
4	0.8	0.9	grey77	square	0.48	0.14
')
pos = as.matrix(subset(frame, select=c(x, y)))
M = matrix(nrow = n.labels, ncol = n.labels, byrow = TRUE, data = 0)
M[2, 1] = "' '"
M[3, 1] = "' '"
M[4, 1] = "' '"
M[4, 2] = "' '"
M[3, 2] = "' '"
M[4, 3] = "' '"

# export
jpeg('figures/confounded_design.jpg', width=5, height=4, units='in', res=400)
par(mai=c(0,0.01,0.01,0.01))
plotmat(M, pos = pos, name = labels, lwd = 1, shadow.size=0, curve=0, arr.pos=0.4, dr=0.2,
        box.lwd = 2, cex.txt = 1, box.size = frame$box.size, box.col=frame$box.col,
        box.type = frame$box.type, box.prop = frame$box.prop, txt.col = 'black')
text(0.25,0.25, 'No funding')
dev.off()