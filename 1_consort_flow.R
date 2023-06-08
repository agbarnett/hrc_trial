# 1_consort_flow.R
# make consort flow diagram of randomised
# June 2021
library(diagram)
library(readxl)
library(dplyr)
library(janitor)

### data
# get all the numbers per year
numbers = read_excel('../data/randomised.xlsx', sheet='Numbers', n_max=7, na = "NA", skip = 1) %>%
  clean_names() %>%
  select(-url_for_media_release)

#
load('data/researchers.RData') # from 0_read_data.R

### numbers
# a) split overall numbers by consent or not and funded or not
nstats = summarise(numbers,
                  consent = sum(consent, na.rm = TRUE),
                  not_consent = sum(not_consent, na.rm = TRUE),
                  consent_not_funded = sum(consent_not_funded, na.rm = TRUE),
                  consent_funded = sum(consent_funded, na.rm = TRUE))
total = sum(numbers$fundable) # total number of applications

# b) split final researchers, if funded or not chose funded, if consent or not chose consent
followup = mutate(researchers,
                    fund_num = ifelse(result=='Funded', 1, 0),
                    consent_num = ifelse(consent =='Yes', 1, 0)) %>%
  group_by(name) %>%
  summarise(consent = max(consent_num),
            fund = max(fund_num))
followup_count = group_by(followup, consent, fund) %>% # count by status
  tally()
follow_not_funded = filter(followup_count, fund==0) %>% pull(n)
follow_funded = sum(filter(followup_count, fund==1) %>% pull(n))

## plot
# labels
l1 = paste('Eligible\napplications (n=', total,')', sep='') 
l2 = paste('Consented\n(n=', nstats$consent,')', sep='') 
l3 = paste('Not consented\n(n=', nstats$not_consent,')', sep='') 
l4 = paste('Not funded\n(n=', nstats$consent_not_funded,')', sep='') 
l5 = paste('Funded\n(n=', nstats$consent_funded,')', sep='') 
l6 = paste('Funded\n(n=', sum(numbers$funded) - nstats$consent_funded,')', sep='') 
l7 = paste('Not funded\n(n=', sum(numbers$not_funded)-  nstats$consent_not_funded,')', sep='') 
l8 = ''
l9 = ''
l10 = ''
l11 = paste('Never funded\n(n=', follow_not_funded, ')', sep='')
l12 = paste('Funded 1 or more times\n(n=', follow_funded, ')', sep='')
labels = c(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12)
n.labels = length(labels)
### make data frame of box chars
frame = read.table(sep='\t', stringsAsFactors=F, skip=0, header=T, text='
i	x	y	box.col	box.type	box.prop	box.size
1	0.5	0.9	white	square	0.36	0.18
2	0.25	0.65	white	square	0.41	0.14
3	0.75	0.65	white	square	0.41	0.14
4	0.15	0.4	darkseagreen1	square	0.48	0.1
5	0.38	0.4	darkseagreen1	square	0.48	0.1
6	0.62	0.4	darkseagreen1	square	0.48	0.1
7	0.85	0.4	grey	square	0.48	0.1
8	0.15	0.1	transparent	ellipse	0.1	0.1
9	0.38	0.1	transparent	ellipse	0.1	0.1
10	0.62	0.1	transparent	ellipse	0.1	0.1
11	0.15	0.1	darkseagreen1	square	0.42	0.12
12	0.53	0.1	darkseagreen1	square	0.20	0.25')
pos = as.matrix(subset(frame, select=c(x, y)))
M = matrix(nrow = n.labels, ncol = n.labels, byrow = TRUE, data = 0)
M[2, 1] = "' '"
M[3, 1] = "' '"
M[4, 2] = "' '"
M[5, 2] = "' '"
M[6, 3] = "' '"
M[7, 3] = "' '"
M[8, 4] = "' '"
M[9, 5] = "' '"
M[10, 6] = "' '"
tcol = rep('black', n.labels)

# 
jpeg('figures/consort.flow.jpg', width=5, height=4, units='in', res=300)
par(mai=rep(0.01, 4))
plotmat(M, pos = pos, name = labels, lwd = 1, shadow.size=0, curve=0,
        box.lwd = 2, cex.txt = 1, box.size = frame$box.size, box.col=frame$box.col,
        box.type = frame$box.type, box.prop = frame$box.prop, txt.col = tcol)
# add text in white box
rect(xleft=0.1, ybottom=0.18, ytop=0.22, xright = 0.75, col = 'white', border=NA)
text(x=0.38, y=0.20, label='Individual researchers for follow-up')
dev.off()

# for slide for talk (larger version)
jpeg('U:/Research/Projects/ihbi/aushsi/aushsi_barnetta/meta.research/presentations/talks/ANZSC/figures/consort.flow.slide.jpg', width=6, height=5, units='in', res=300)
par(mai=rep(0.01, 4))
plotmat(M, pos = pos, name = labels, lwd = 1, shadow.size=0, curve=0, 
        box.lwd = 2, cex.txt = 1.4, box.size = frame$box.size, box.col=frame$box.col,
        box.type = frame$box.type, box.prop = frame$box.prop, txt.col = tcol)
# add text in white box
rect(xleft=0.1, ybottom=0.18, ytop=0.22, xright = 0.75, col = 'white', border=NA)
text(x=0.38, y=0.20, label='Individual researchers for follow-up')
dev.off()


## check numbers for consented but not-funded researchers
not_funded = filter(followup, consent==1, fund==0)
apps_from_not_funded = left_join(not_funded, researchers, by='name')
cat('Consented but never funded = ', nrow(not_funded),'.\n', sep='')
cat('And these people had this many applications = ', nrow(apps_from_not_funded),'.\n', sep='')
