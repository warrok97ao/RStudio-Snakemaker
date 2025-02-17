# RStudio Snakemaker
This is an RStudio addin that leverage an LLM to generate snakemake rules using either bash or R commands. 
## Installation: 
```
install.packages("devtools")

devtools::install_github("warrok97ao/RStudio-Snakemaker")
```
In order to launch the addin: 
Select the Addin in the RStudio menu, then select "Snakemaker"

<img width="245" alt="Schermata 2025-02-17 alle 14 19 38" src="https://github.com/user-attachments/assets/5ec21daf-25f3-4c46-9f51-4e72172c0db7" />

The addin will open as a shiny app in the viewer page in the right part of the RStudio interface. 

<img width="588" alt="Schermata 2025-02-17 alle 14 56 35" src="https://github.com/user-attachments/assets/2ba3547d-b5c0-434a-ba73-03392ad6355f" />

## First time utilization: 
When you run the addin for the first time, a default language model is selected, but can be changed in the Configuration button inside the addin.
When you generate your first rule, it will ask to insert a API key in order to continue. This API key will be stored as an environmental variable in your machine, and will allow the addin to work.

This API key will not be asked again, but when selecting different models that requires different API key, the prompt will ask for the new key.  

<img width="286" alt="Schermata 2025-02-17 alle 15 00 38" src="https://github.com/user-attachments/assets/b468de74-4dd3-4680-86fd-55185eedde48" />

If the API key needs to be changed, in the Configuration menu there is a button to delete the current saved one and the user will be asked to insert the new API key the next time a rule is generated. 

<img width="356" alt="Schermata 2025-02-17 alle 15 04 21" src="https://github.com/user-attachments/assets/13b5b99f-5448-415a-a0ac-d792a8f91fcf" />





