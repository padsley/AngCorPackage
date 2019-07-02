BEGIN{}
{
    #theta_alpha=substr(FILENAME,10,3)
    #phi_gamma=substr(FILENAME,14,3)
    
    theta_alpha=substr(FILENAME,17,3)
    phi_gamma=substr(FILENAME,21,3)
    
    #print theta_alpha
    #print phi_gamma
    
    test=substr(phi_gamma,3,1)
    test2=substr(phi_gamma,2,1)
    
    if(test==".") {phi_gamma=substr(FILENAME,21,2)}
    if(test2==".") {phi_gamma=substr(FILENAME,21,1)}
    #print phi_gamma
    if(theta_alpha=="10.")
    { 
        #     print "theta_alpha>=10."
        theta_alpha=substr(FILENAME,17,4);
        phi_gamma=substr(FILENAME,22,2);
        
        test3=substr(phi_gamma,2,1);
        if(test3==".") phi_gamma=substr(FILENAME,22,1)
    }
    
    print theta_alpha,$2*1,phi_gamma,$3*1
    
}
END{}