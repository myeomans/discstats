
mymodel <- loess( hr ~ p_x + p_z ) 
#mymodel <- lm(v_ang ~ p_z + pfx_z )
myx <- matrix(data=seq(from=-2,to=2,length=20),nrow=20,ncol=25) 
myz <- t (matrix(data=seq(from=-2,to=2,length=20),nrow=20,ncol=25)) 
mypredict<-matrix(data=mypredict,nrow=20,nrow=25) 
image(x=seq(from=-2,to=2,length=20), 
      y=seq(from=0,to=5,length=25), 
      z=mypredict, 
      col=… 
) 
mypredict <- predict(object=mymodel, 
                     newdata=data.frame(p_x=as.vector(myx), 
                                        p_z=as.vector(myz) 