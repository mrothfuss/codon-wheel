#!/usr/bin/env Rscript

plotlim = 3
s1 = 8
s2 = 4
s3 = 1.6
s4 = 1.6
s5 = 1.6
l1 = 0.6
l2 = 0.3
l3 = 0.1
l4 = 0.1
l5 = 0.07
lwd = 4
fgcol = "white"
bgcol = "#003153"
guides = FALSE
grouped = TRUE

library(plotrix)

ang_to_codon = function(ang) {
	# output
	codon = ""

	# letter 1
	ind = floor((ang %% (360/1)) / (360/4)) + 1
	if(grouped) {
		lets = c("T", "A", "G", "C")
	} else {
		lets = c("A", "G", "C", "T")
	}
	let1 = lets[ind]
	codon = paste(codon, let1, sep="")

	# letter 2
	ind = floor((ang %% (360/4)) / (90 / 4)) + 1
	lets = c()
	if(grouped) {
		if(let1 == "T") {
			lets = c("T", "A", "G", "C")
		}   
		if(let1 == "G" ) { 
			lets = c("T", "C", "A", "G")
		}   
		if(let1 == "C") {
			lets = c("C", "G", "A", "T")
		}   
		if(!length(lets)) {
			lets = c("G", "A", "C", "T")
		}   
	} else {
		if(let1 == "A") {
			lets = c("G", "A", "C", "T")
		}
		if(let1 == "T") {
			lets = c("T", "G", "A", "C")
		}
		if(let1 == "G" ) {
			lets = c("C", "T", "G", "A")
		}
		if(!length(lets)) {
			lets = c("G", "A", "C", "T")
		}
	}
	let2 = lets[ind]
	codon = paste(codon, let2, sep="")

	# letter 3
	ind = floor((ang %% (90/4)) / (22.5 / 4)) + 1
	lets = c()
	if(grouped) {
		if(let1 == "T" && let2 == "G") {
			lets = c("A", "G", "C", "T")
		}
		if(let1 == "T" && let2 == "A") {
			lets = c("C", "T", "G", "A")
		}
		if(let1 == "A" && let2 == "G") {
			lets = c("C", "T", "G", "A")
		}
		if(let1 == "C" && let2 == "A") {
			lets = c("C", "T", "G", "A")
		}
		if(!length(lets)) {
			lets = c("G", "A", "C", "T")
		}
	} else {
		if(let1 == "T" && let2 == "G") {
			lets = c("C", "T", "G", "A")
		}
		if(let1 == "T" && let2 == "A") {
			lets = c("G", "A", "C", "T")
		}
		if(let1 == "A" && let2 == "A") {
			lets = c("C", "T", "G", "A")
		}
		if(let1 == "A" && let2 == "G") {
			lets = c("C", "T", "G", "A")
		}
		if(!length(lets)) {
			lets = c("G", "A", "C", "T")
		}
	}
	let3 = lets[ind]
	codon = paste(codon, let3, sep="")

	return(codon)
}

codon_score = list()
codon_resid = list()
codon="TGG"; codon_score[[codon]] = 9; codon_resid[[codon]] = "W"
codon="TGA"; codon_score[[codon]] = 4; codon_resid[[codon]] = "■"
codon="TGC"; codon_score[[codon]] = 9; codon_resid[[codon]] = "C"
codon="TGT"; codon_score[[codon]] = 7; codon_resid[[codon]] = "C"
codon="TAG"; codon_score[[codon]] = 1; codon_resid[[codon]] = "■"
codon="TAA"; codon_score[[codon]] = 9; codon_resid[[codon]] = "■"
codon="TAC"; codon_score[[codon]] = 4; codon_resid[[codon]] = "Y"
codon="TAT"; codon_score[[codon]] = 9; codon_resid[[codon]] = "Y"
codon="TCG"; codon_score[[codon]] = 7; codon_resid[[codon]] = "S"
codon="TCA"; codon_score[[codon]] = 4; codon_resid[[codon]] = "S"
codon="TCC"; codon_score[[codon]] = 5; codon_resid[[codon]] = "S"
codon="TCT"; codon_score[[codon]] = 5; codon_resid[[codon]] = "S"
codon="TTG"; codon_score[[codon]] = 3; codon_resid[[codon]] = "L"
codon="TTA"; codon_score[[codon]] = 3; codon_resid[[codon]] = "L"
codon="TTC"; codon_score[[codon]] = 6; codon_resid[[codon]] = "F"
codon="TTT"; codon_score[[codon]] = 9; codon_resid[[codon]] = "F"
codon="GGG"; codon_score[[codon]] = 4; codon_resid[[codon]] = "G"
codon="GGA"; codon_score[[codon]] = 2; codon_resid[[codon]] = "G"
codon="GGC"; codon_score[[codon]] = 9; codon_resid[[codon]] = "G"
codon="GGT"; codon_score[[codon]] = 7; codon_resid[[codon]] = "G"
codon="GAG"; codon_score[[codon]] = 6; codon_resid[[codon]] = "E"
codon="GAA"; codon_score[[codon]] = 9; codon_resid[[codon]] = "E"
codon="GAC"; codon_score[[codon]] = 5; codon_resid[[codon]] = "D"
codon="GAT"; codon_score[[codon]] = 9; codon_resid[[codon]] = "D"
codon="GCG"; codon_score[[codon]] = 9; codon_resid[[codon]] = "A"
codon="GCA"; codon_score[[codon]] = 5; codon_resid[[codon]] = "A"
codon="GCC"; codon_score[[codon]] = 7; codon_resid[[codon]] = "A"
codon="GCT"; codon_score[[codon]] = 4; codon_resid[[codon]] = "A"
codon="GTG"; codon_score[[codon]] = 9; codon_resid[[codon]] = "V"
codon="GTA"; codon_score[[codon]] = 3; codon_resid[[codon]] = "V"
codon="GTC"; codon_score[[codon]] = 4; codon_resid[[codon]] = "V"
codon="GTT"; codon_score[[codon]] = 5; codon_resid[[codon]] = "V"
codon="AGG"; codon_score[[codon]] = 1; codon_resid[[codon]] = "R"
codon="AGA"; codon_score[[codon]] = 1; codon_resid[[codon]] = "R"
codon="AGC"; codon_score[[codon]] = 9; codon_resid[[codon]] = "S"
codon="AGT"; codon_score[[codon]] = 6; codon_resid[[codon]] = "S"
codon="AAG"; codon_score[[codon]] = 3; codon_resid[[codon]] = "K"
codon="AAA"; codon_score[[codon]] = 9; codon_resid[[codon]] = "K"
codon="AAC"; codon_score[[codon]] = 7; codon_resid[[codon]] = "N"
codon="AAT"; codon_score[[codon]] = 9; codon_resid[[codon]] = "N"
codon="ACG"; codon_score[[codon]] = 5; codon_resid[[codon]] = "T"
codon="ACA"; codon_score[[codon]] = 2; codon_resid[[codon]] = "T"
codon="ACC"; codon_score[[codon]] = 9; codon_resid[[codon]] = "T"
codon="ACT"; codon_score[[codon]] = 3; codon_resid[[codon]] = "T"
codon="ATG"; codon_score[[codon]] = 9; codon_resid[[codon]] = "M"
codon="ATA"; codon_score[[codon]] = 1; codon_resid[[codon]] = "I"
codon="ATC"; codon_score[[codon]] = 8; codon_resid[[codon]] = "I"
codon="ATT"; codon_score[[codon]] = 9; codon_resid[[codon]] = "I"
codon="CGG"; codon_score[[codon]] = 2; codon_resid[[codon]] = "R"
codon="CGA"; codon_score[[codon]] = 1; codon_resid[[codon]] = "R"
codon="CGC"; codon_score[[codon]] = 9; codon_resid[[codon]] = "R"
codon="CGT"; codon_score[[codon]] = 8; codon_resid[[codon]] = "R"
codon="CAG"; codon_score[[codon]] = 9; codon_resid[[codon]] = "Q"
codon="CAA"; codon_score[[codon]] = 5; codon_resid[[codon]] = "Q"
codon="CAC"; codon_score[[codon]] = 7; codon_resid[[codon]] = "H"
codon="CAT"; codon_score[[codon]] = 9; codon_resid[[codon]] = "H"
codon="CCG"; codon_score[[codon]] = 9; codon_resid[[codon]] = "P"
codon="CCA"; codon_score[[codon]] = 3; codon_resid[[codon]] = "P"
codon="CCC"; codon_score[[codon]] = 1; codon_resid[[codon]] = "P"
codon="CCT"; codon_score[[codon]] = 2; codon_resid[[codon]] = "P"
codon="CTG"; codon_score[[codon]] = 9; codon_resid[[codon]] = "L"
codon="CTA"; codon_score[[codon]] = 1; codon_resid[[codon]] = "L"
codon="CTC"; codon_score[[codon]] = 2; codon_resid[[codon]] = "L"
codon="CTT"; codon_score[[codon]] = 2; codon_resid[[codon]] = "L"

#png("chart.png", width=2000, height=2000, bg=bgcol)
svg("chart.svg", width=30, height=30, bg=bgcol)
par(mar=c(0,0,0,0), font=2)
plot(NA, NA, xlim=c(-plotlim, plotlim), ylim=c(-plotlim, plotlim), xlab=NA, ylab=NA, axes=FALSE)

lt = l1 + l2 + l3 +l4
for(ang in seq(from=0, to=359, by=360/64)) {
	l = l3 + l4
	if(ang %% (360/16) == 0) {
		l = l + l2
	}
	if(ang %% (360/4) == 0) {
		l = l + l1
	}
	x1 = (lt - l) * cos(ang * pi/180)
	y1 = (lt - l) * sin(ang * pi/180)
	x2 = (lt - 0.005) * cos(ang * pi/180)
	y2 = (lt - 0.005) * sin(ang * pi/180)
	lines(c(x1, x2), c(y1, y2), lwd=lwd, col=fgcol)
}
draw.circle(0,0,l1, lwd=lwd, border=fgcol)
draw.circle(0,0,l1+l2, lwd=lwd, border=fgcol)
draw.circle(0,0,l1+l2+l3, lwd=lwd, border=fgcol)
draw.circle(0,0,l1+l2+l3+l4, lwd=lwd, border=fgcol)

lt = l1 + l2 + l3 + l4 + l5*2
for(ang in seq(from=0, to=359, by=360/64)) {
	x1 = (l1 + l2 + l3 + l4) * cos(ang * pi/180)
	y1 = (l1 + l2 + l3 + l4) * sin(ang * pi/180)
	x2 = lt * cos(ang * pi/180)
	y2 = lt * sin(ang * pi/180)
	aa_prev = codon_resid[[ang_to_codon(ang-1)]]
	aa_next = codon_resid[[ang_to_codon(ang+1)]]
	if(aa_prev != aa_next) {
		lines(c(x1, x2), c(y1, y2), lwd=lwd, col=fgcol)
	}
}


# codon let1
for(ang in seq(from=360/8, to=359, by=360/4)) {
	# 0.5 / 2
	l = l1 / 2 * 1.1
	x = l * cos(ang * pi/180)
	y = l * sin(ang * pi/180)
	let = substr(ang_to_codon(ang), 1, 1)
	text(x, y, let, cex=s1, col=fgcol)
}

# codon let2
for(ang in seq(from=360/32, to=359, by=360/16)) {
	l = l1 + l2 / 2
	x = l * cos(ang * pi/180)
	y = l * sin(ang * pi/180)
	let = substr(ang_to_codon(ang), 2, 2)
	text(x, y, let, cex=s2, col=fgcol)
}

# codon let3
for(ang in seq(from=360/128, to=359, by=360/64)) {
	l = l1 + l2 + l3/2
	x = l * cos(ang * pi/180)
	y = l * sin(ang * pi/180)
	let = substr(ang_to_codon(ang), 3, 3)
	text(x, y, let, cex=s3, col=fgcol)
}

# codon score
for(ang in seq(from=360/128, to=359, by=360/64)) {
	l = l1 + l2 + l3 + l4/2
	x = l * cos(ang * pi/180)
	y = l * sin(ang * pi/180)
	text(x, y, codon_score[[ang_to_codon(ang)]], cex=s3, col=fgcol)
	#text(x, y, codon_resid[[ang_to_codon(ang)]], cex=s3, col=fgcol)
}

# codon residue
prev_res = codon_resid[[ang_to_codon(359)]]
for(ang in seq(from=360/128, to=359, by=360/64)) {
	this_res = codon_resid[[ang_to_codon(ang)]]
	if(prev_res != this_res) {
		for(ang2 in seq(from=360/64, to=359, by=360/64)) {
			next_res = codon_resid[[ang_to_codon(ang + ang2)]]
			if(next_res != this_res) {
				mid_ang = ang + (ang2 / 2) - 360/128
				l = l1 + l2 + l3 + l4 + l5
				x = l * cos(mid_ang * pi/180)
				y = l * sin(mid_ang * pi/180)
				text(x, y, codon_resid[[ang_to_codon(ang)]], cex=s3, col=fgcol)
				if(guides == TRUE) {
					x1 = (l+0.15) * cos(mid_ang * pi/180)
					y1 = (l+0.15) * sin(mid_ang * pi/180)
					x2 = (l+0.9) * cos(mid_ang * pi/180)
					y2 = (l+0.9) * sin(mid_ang * pi/180)
					lines(c(x1, x2), c(y1, y2), lwd=3, col=fgcol)
				}
				break
			}
		}
		prev_res = this_res
	}
}

# 5' circle
draw.circle(0,0,0.08, lwd=lwd, border=fgcol, col=bgcol)
text(0, 0, "5", cex=2, col=fgcol, adj=c(0.5, 0.5))
#text(0, 0, "   '", cex=2, col=fgcol, adj=c(0.5, 0.5))

graphics.off()
