#Submodel calculate the shadow due to cover 

#Cover effect on shadow
shadow <- shadow + Au * cover   #Assume cover is a precent and ignore solar angle

shadow <- ifelse(shadow > Au, Au, shadow) #no larger than manure surface area. 