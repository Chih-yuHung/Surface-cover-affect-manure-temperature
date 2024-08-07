#Submodel calculate the shadow due to cover 

#Cover effect on shadow
shadow <- coalesce(shadow,0) + Au * cover   #Assume cover is a percent and ignore solar angle

shadow <- ifelse(shadow > Au, Au, shadow)   #No larger than manure surface area