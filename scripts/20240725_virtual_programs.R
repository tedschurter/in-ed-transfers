# public school corporations with high count of other corporations sending transfer students that have identifiable virtual programs

pub_virtual <- tibble(
  corp = c(
    "Union School Corporation",
    "Madison-Grant United School Corporation",
    "Clarksville Community School Corporation",
    "Cloverdale Community Schools",
    "Metropolitan School District Wabash County Schools",
    "Metropolitan School District Wayne Township",
    "Bartholomew Consolidated School Corporation"
    
    
    ),
  
  partner = c(
    "K12 Inc",
    "Virtual Preparatory Academy of Indiana",
    "K12 Inc",
    "EdOptions Academy",
    "Edmentum",
    "Achieve Virtual Education",
    "Edmentum"
    
  ), 
  
  online_name = c(
    "Indiana Digital Learning School of Union School Corporation",
    "Virtual Preparatory Academy of Indiana at Madison-Grant",
    "Indiana Gateway Digital Academy",
    "Cloverdale Virtual Success Academy",
    "Bridge",
    "Achieve Virtual Education",
    "Columbus Virtual Pathway"
  ),
  
  url = c(
    "https://indls.k12.com/",
    "https://indianamg.virtualpreparatoryacademy.com/",
    "https://www.clarksvilleschools.org/indiana-digital-gateway-academy/",
    "https://www.cloverdale.k12.in.us/virtual_academy",
    "https://whites.msdwc.org/bridge/home",
    "https://achievevirtual.org/",
    "https://www.bcscschools.org/cvp"
  )
  
)

pub_virtual



