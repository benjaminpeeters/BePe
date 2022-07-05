


#' @export
plotcorrelation <- function(db, type = 'corrplot')
{
	
	order=hclust(dist(Cor(db)))$order
	db = db[order,]
	M = Cor(db)
	
	if(type == 'corrplot'){
		
		
#		corrplot.mixed(Cor(db), diag='n', order = 'AOE')
		# corrplot(Cor(db), diag=FALSE, order = 'FPC')
		
		test = cor.mtest(db, conf.level = 0.95)
		corrplot(Cor(db), p.mat = test$p, sig.level = 0.10, order = 'hclust', addrect = 2, insig='blank', diag=FALSE)
		
		
#		corrplot(M, p.mat = test$p, method = 'circle', type = 'lower', insig='blank',
#			addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag=FALSE)
		
	}else{
		
		panel.cor <- function(x, y){
			
			r <- round(Cor(x, y), digits=2)
	#		col = rgb((colorRamp(c("blue", "red"))(r))/255)
			
		    usr <- par("usr"); on.exit(par(usr))
		    par(usr = c(0, 1, 0, 1))
		    
		    txt <- paste0(r)
		    cex.cor <- 0.8/strwidth(txt)
		    text(0.5, 0.5, txt)
	#	    legend('center', txt, col = 'red')
		}
	#		 Customize upper panel
		upper.panel<-function(x, y){
		  points(x,y, pch = 19) # , col =db[,1]
		}
	#		 Create the plots
		pairs(db,  lower.panel = panel.cor, upper.panel = upper.panel)
		
	}
	
}



