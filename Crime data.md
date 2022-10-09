```R
#In this project I'm going to explore murder data in the US. I want to answer  5 questions about it:
#1) Which states were doing better in decreasing murder numbers?
#2) When did murders hit  maximum?
#3) Which states were doing better in decreasing the level of murders (murders/population), and were these states the same as in the first question
#4) What states had the lowest murder rate for a long time?
#5) Does it correlate with the GFI(Gun Friendly Index)?
```


```R

```


```R

```


```R
install.packages(c("readr", "readxl", "dplyr", "ggrepel","data.table", "timechange", "haven", "tidyr", "stringr","scales", "lubridate", "ggplot2", "scales", "janitor", "devtools"))

library(readr)
library(dplyr)
library(data.table)
library(timechange)
library(stringr)
library(lubridate)
library(ggplot2)
library(scales)
library(devtools)
install_github("ProcessMiner/nlcor")
library(nlcor)
library(haven)
library(readxl)
library(tidyr)
library(scales)
library(ggrepel)
```

    
    The downloaded binary packages are in
    	/var/folders/dz/2djfbpx56bl18bsf7qtl8x840000gn/T//RtmpBe2ocD/downloaded_packages


    
    Attaching package: ‘dplyr’
    
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    
    
    Attaching package: ‘data.table’
    
    
    The following objects are masked from ‘package:dplyr’:
    
        between, first, last
    
    
    
    Attaching package: ‘lubridate’
    
    
    The following objects are masked from ‘package:data.table’:
    
        hour, isoweek, mday, minute, month, quarter, second, wday, week,
        yday, year
    
    
    The following objects are masked from ‘package:base’:
    
        date, intersect, setdiff, union
    
    
    
    Attaching package: ‘scales’
    
    
    The following object is masked from ‘package:readr’:
    
        col_factor
    
    
    Loading required package: usethis
    
    Skipping install of 'nlcor' from a github remote, the SHA1 (92d4aa3c) has not changed since last install.
      Use `force = TRUE` to force installation
    



```R

```


```R

```


```R
crime_data_start <- read_sav("/Users/daniel/Downloads/SHR76_20.sav")
```


```R
crime_data_start$State  <- as.factor(crime_data_start$State)



```


```R
str(crime_data_start)
```

    tibble [827,219 × 31] (S3: tbl_df/tbl/data.frame)
     $ ID          : chr [1:827219] "197609001AL00400" "197701001AL00400" "197703001AL00400" "197703001AL00401" ...
      ..- attr(*, "label")= chr "ID (MAP GENERATED)"
      ..- attr(*, "format.spss")= chr "A16"
      ..- attr(*, "display_width")= int 20
     $ CNTYFIPS    : chr+lbl [1:827219] 01001, 01001, 01001, 01001, 01001, 01001, 01001, 01...
       ..@ label        : chr "COUNTY NAME"
       ..@ format.spss  : chr "A15"
       ..@ display_width: int 19
       ..@ labels       : Named chr [1:3202] "ZZ000" "02100" "51600" "51700" ...
       .. ..- attr(*, "names")= chr [1:3202] "Foreign Resident" "Haines, AK" "Fairfax, VA" "Newport News, VA" ...
     $ Ori         : chr [1:827219] "AL00400" "AL00400" "AL00400" "AL00401" ...
      ..- attr(*, "label")= chr "ORI CODE"
      ..- attr(*, "format.spss")= chr "A7"
     $ State       : Factor w/ 52 levels "AK","AL","AR",..: 2 2 2 2 2 2 2 2 2 2 ...
     $ Agency      : chr [1:827219] "Autauga County" "Autauga County" "Autauga County" "Prattville" ...
      ..- attr(*, "label")= chr "AGENCY NAME"
      ..- attr(*, "format.spss")= chr "A450"
      ..- attr(*, "display_width")= int 27
     $ Agentype    : dbl+lbl [1:827219] 1, 1, 1, 3, 1, 1, 1, 1, 1, 3, 1, 1, 3, 3, 3, 3, 3, ...
       ..@ label        : chr "TYPE OF AGENCY"
       ..@ format.spss  : chr "F1.0"
       ..@ display_width: int 13
       ..@ labels       : Named num [1:8] 1 2 3 5 6 7 8 9
       .. ..- attr(*, "names")= chr [1:8] "Sheriff" "County police" "Municipal police" "Primary state LE" ...
     $ Source      : dbl+lbl [1:827219] 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
       ..@ label        : chr "DATA SOURCE: FBI OR MAP FOIA"
       ..@ format.spss  : chr "F1.0"
       ..@ display_width: int 5
       ..@ labels       : Named num [1:2] 0 1
       .. ..- attr(*, "names")= chr [1:2] "MAP" "FBI"
     $ Solved      : dbl+lbl [1:827219] 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, ...
       ..@ label      : chr "WAS OFFENDER IDENTIFIED"
       ..@ format.spss: chr "F1.0"
       ..@ labels     : Named num [1:2] 0 1
       .. ..- attr(*, "names")= chr [1:2] "No" "Yes"
     $ Year        : num [1:827219] 1976 1977 1977 1977 1977 ...
      ..- attr(*, "label")= chr "YEAR"
      ..- attr(*, "format.spss")= chr "F4.0"
      ..- attr(*, "display_width")= int 6
     $ StateName   : chr [1:827219] "" "" "" "" ...
      ..- attr(*, "label")= chr "STATE NAME"
      ..- attr(*, "format.spss")= chr "A6"
      ..- attr(*, "display_width")= int 16
     $ Month       : dbl+lbl [1:827219]  9,  1,  3,  3,  8, 10,  5, 12,  2, 12,  8,  9,  1,...
       ..@ label      : chr "MONTH OF OFFENSE"
       ..@ format.spss: chr "F2.0"
       ..@ labels     : Named num [1:12] 1 2 3 4 5 6 7 8 9 10 ...
       .. ..- attr(*, "names")= chr [1:12] "January" "February" "March" "April" ...
     $ Incident    : num [1:827219] 1 1 1 1 1 1 1 1 1 1 ...
      ..- attr(*, "label")= chr "INCIDENT NUMBER"
      ..- attr(*, "format.spss")= chr "F3.0"
      ..- attr(*, "display_width")= int 6
     $ ActionType  : dbl+lbl [1:827219] 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, ...
       ..@ label        : chr "TYPE OF ACTION"
       ..@ format.spss  : chr "F1.0"
       ..@ display_width: int 12
       ..@ labels       : Named num [1:2] 0 1
       .. ..- attr(*, "names")= chr [1:2] "Normal update" "Adjustment"
     $ Homicide    : chr+lbl [1:827219] A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, ...
       ..@ label        : chr "TYPE OF OFFENSE:HOMICIDE"
       ..@ format.spss  : chr "A1"
       ..@ display_width: int 18
       ..@ labels       : Named chr [1:2] "A" "B"
       .. ..- attr(*, "names")= chr [1:2] "Murder and non-negligent manslaughter" "Manslaughter by negligence"
     $ Situation   : chr+lbl [1:827219] B, A, C, A, A, A, C, A, B, A, A, A, A, A, A, A, A, ...
       ..@ label        : chr "SITUATION"
       ..@ format.spss  : chr "A1"
       ..@ display_width: int 25
       ..@ labels       : Named chr [1:6] "A" "B" "C" "D" ...
       .. ..- attr(*, "names")= chr [1:6] "Single victim/single offender" "Single victim/unknown offender(s)" "Single victim/multiple offenders" "Multiple victims/single offender" ...
     $ VicAge      : num [1:827219] 30 65 48 27 17 62 54 48 999 51 ...
      ..- attr(*, "label")= chr "VICTIM AGE"
      ..- attr(*, "format.spss")= chr "F3.0"
      ..- attr(*, "display_width")= int 10
     $ VicSex      : chr+lbl [1:827219] M, F, M, M, F, M, M, F, F, M, M, M, M, M, M, M, M, ...
       ..@ label        : chr "VICTIM SEX"
       ..@ format.spss  : chr "A1"
       ..@ display_width: int 7
       ..@ labels       : Named chr [1:3] "F" "M" "U"
       .. ..- attr(*, "names")= chr [1:3] "Female" "Male" "Unknown"
     $ VicRace     : chr+lbl [1:827219] B, B, W, B, B, A, B, W, U, B, B, B, W, B, W, B, W, ...
       ..@ label        : chr "VICTIM RACE"
       ..@ format.spss  : chr "A1"
       ..@ display_width: int 24
       ..@ labels       : Named chr [1:6] "A" "B" "I" "P" ...
       .. ..- attr(*, "names")= chr [1:6] "Asian" "Black" "American Indian or Alaskan Native" "Native Hawaiian or Pacific Islander" ...
     $ VicEthnic   : chr+lbl [1:827219] U, U, U, U, U, U, U, U, U, U, U, U, H, H, N, N, N, ...
       ..@ label        : chr "VICTIM ETHNIC ORIGIN"
       ..@ format.spss  : chr "A1"
       ..@ display_width: int 13
       ..@ labels       : Named chr [1:3] "H" "N" "U"
       .. ..- attr(*, "names")= chr [1:3] "Hispanic origin" "Not of Hispanic origin" "Unknown or not reported"
     $ OffAge      : num [1:827219] 999 62 52 22 21 80 54 26 999 24 ...
      ..- attr(*, "label")= chr "OFFENDER AGE"
      ..- attr(*, "format.spss")= chr "F3.0"
      ..- attr(*, "display_width")= int 12
     $ OffSex      : chr+lbl [1:827219] U, M, M, F, M, M, F, M, U, F, M, M, M, M, M, M, M, ...
       ..@ label        : chr "OFFENDER SEX"
       ..@ format.spss  : chr "A1"
       ..@ display_width: int 10
       ..@ labels       : Named chr [1:3] "F" "M" "U"
       .. ..- attr(*, "names")= chr [1:3] "Female" "Male" "Unknown"
     $ OffRace     : chr+lbl [1:827219] U, B, W, B, B, B, B, W, U, B, B, W, W, B, W, B, W, ...
       ..@ label        : chr "OFFENDER RACE"
       ..@ format.spss  : chr "A1"
       ..@ display_width: int 9
       ..@ labels       : Named chr [1:6] "A" "B" "I" "P" ...
       .. ..- attr(*, "names")= chr [1:6] "Asian" "Black" "American Indian or Alaskan Native" "Native Hawaiian or Pacific Islander" ...
     $ OffEthnic   : chr+lbl [1:827219] U, U, U, U, U, U, U, U, U, U, U, U, H, U, N, N, N, ...
       ..@ label        : chr "OFFENDER ETHNIC ORIGIN"
       ..@ format.spss  : chr "A1"
       ..@ display_width: int 23
       ..@ labels       : Named chr [1:3] "H" "N" "U"
       .. ..- attr(*, "names")= chr [1:3] "Hispanic origin" "Not of Hispanic origin" "Unknown or not reported"
     $ Weapon      : dbl+lbl [1:827219] 90, 90, 12, 14, 20, 14, 20, 30, 90, 20, 13, 12, 20,...
       ..@ label        : chr "WEAPON"
       ..@ format.spss  : chr "F2.0"
       ..@ display_width: int 25
       ..@ labels       : Named num [1:17] 11 12 13 14 15 20 30 40 50 55 ...
       .. ..- attr(*, "names")= chr [1:17] "Firearm, type not stated" "Handgun - pistol, revolver, etc" "Rifle" "Shotgun" ...
     $ Relationship: chr+lbl [1:827219] UN, AQ, AQ, HU, AQ, ST, HU, AQ, UN, CH, AQ, AQ, AQ,...
       ..@ label        : chr "RELATIONSHIP"
       ..@ format.spss  : chr "A2"
       ..@ display_width: int 22
       ..@ labels       : Named chr [1:29] "DA" "FA" "SD" "EE" ...
       .. ..- attr(*, "names")= chr [1:29] "Daughter" "Father" "Stepdaughter" "Employee" ...
     $ Circumstance: dbl+lbl [1:827219] 60, 42, 45, 45, 99,  3, 99, 60, 99, 45, 45, 45, 60,...
       ..@ label        : chr "CIRCUMSTANCE"
       ..@ format.spss  : chr "F2.0"
       ..@ display_width: int 29
       ..@ labels       : Named num [1:32] 2 3 5 6 7 9 10 17 18 19 ...
       .. ..- attr(*, "names")= chr [1:32] "Rape" "Robbery" "Burglary" "Larceny" ...
     $ Subcircum   : chr+lbl [1:827219]  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  ,  , ...
       ..@ label        : chr "SUB-CIRCUMSTANCE"
       ..@ format.spss  : chr "A1"
       ..@ display_width: int 37
       ..@ labels       : Named chr [1:7] "A" "B" "C" "D" ...
       .. ..- attr(*, "names")= chr [1:7] "Felon attacked police officer" "Felon attacked fellow police officer" "Felon attacked a civilian" "Felon attempted flight from a crime" ...
     $ VicCount    : num [1:827219] 0 0 0 0 0 0 0 0 0 0 ...
      ..- attr(*, "label")= chr "ADDITIONAL VICTIM COUNT"
      ..- attr(*, "format.spss")= chr "F3.0"
     $ OffCount    : num [1:827219] 0 0 1 0 0 0 2 0 0 0 ...
      ..- attr(*, "label")= chr "ADDITIONAL OFFENDER COUNT"
      ..- attr(*, "format.spss")= chr "F3.0"
      ..- attr(*, "display_width")= int 5
     $ FileDate    : chr [1:827219] "030180" "030180" "030180" "030180" ...
      ..- attr(*, "label")= chr "DATE FILE SENT TO FBI"
      ..- attr(*, "format.spss")= chr "A6"
      ..- attr(*, "display_width")= int 6
     $ MSA         : dbl+lbl [1:827219] 33860, 33860, 33860, 33860, 33860, 33860, 33860, 33...
       ..@ label        : chr "METROPOLITAN AREA"
       ..@ format.spss  : chr "F8.0"
       ..@ display_width: int 21
       ..@ labels       : Named num [1:411] 10180 10420 10500 10580 10740 ...
       .. ..- attr(*, "names")= chr [1:411] "Abilene, TX" "Akron, OH" "Albany, GA" "Albany-Schenectady-Troy, NY" ...



```R
#Working with minimum data 
crime_data_start <- crime_data_start %>%
select(State,  Year, StateName) %>%
filter(State!= "PAPSP8")
```


```R
crime_data <- crime_data_start %>%
count(State, Year) 

crime_data$Year <- as.Date(as.character(crime_data$Year),  format = "%Y")

crime_data_max <- crime_data %>%
group_by(State) %>%
summarize(max = max(n))
head(crime_data_max)
```


<table class="dataframe">
<caption>A tibble: 6 × 2</caption>
<thead>
	<tr><th scope=col>State</th><th scope=col>max</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>AK</td><td>  78</td></tr>
	<tr><td>AL</td><td> 575</td></tr>
	<tr><td>AR</td><td> 340</td></tr>
	<tr><td>AZ</td><td> 523</td></tr>
	<tr><td>CA</td><td>4370</td></tr>
	<tr><td>CO</td><td> 349</td></tr>
</tbody>
</table>




```R
crime_data_max <- crime_data_max %>%
left_join(crime_data, by = c('State', "max" = "n"))

```


```R
ggplot(crime_data, aes(x= Year, y = n)) + 
geom_line() + 
theme(
  axis.text.x = element_blank()) +
facet_wrap(vars(State), scales = "free_y")

```


    
![png](YearVsNumbeerOfCrimes.png)
    



```R
pop <- read_excel("/Users/daniel/Desktop/Projects/sources_crime/pop-decennial.xls", sheet = "States", skip =3, col_names = TRUE)
```


```R
state_code <- fread("/Users/daniel/Desktop/Projects/sources_crime/us-states-territories.csv")
```


```R
pop <- pop[-1,]
```


```R
pop <- pop %>%
select(2, 16:20)
pop$Areaname <- as.factor(pop$Areaname)

```


```R
#Changing column with state name to the abbreviations
state_code$Name <- as.character(state_code$Name)
state_code$Name <- str_remove(state_code$Name, pattern = "\\s")
state_code$Name <- str_replace(state_code$Name, "Districtof Columbia", "DistrictofColumbia")
pop$Areaname <- str_remove(pop$Areaname, pattern = "\\s")
pop$Areaname <- str_replace(pop$Areaname, "Districtof Columbia", "DistrictofColumbia")




```


```R
pop <- pop %>%
left_join(state_code, by = c("Areaname" = "Name")) %>%
select(2:6, "Abbreviation")


```


```R
pop


```


<table class="dataframe">
<caption>A tibble: 51 × 6</caption>
<thead>
	<tr><th scope=col>1980</th><th scope=col>1990</th><th scope=col>2000</th><th scope=col>2010</th><th scope=col>2020</th><th scope=col>Abbreviation</th></tr>
	<tr><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;chr&gt;</th></tr>
</thead>
<tbody>
	<tr><td> 3894025</td><td> 4040587</td><td> 4447100</td><td> 4779736</td><td> 5024279</td><td>AL</td></tr>
	<tr><td>  401851</td><td>  550043</td><td>  626932</td><td>  710231</td><td>  733391</td><td>AK</td></tr>
	<tr><td> 2716546</td><td> 3665228</td><td> 5130632</td><td> 6392017</td><td> 7151502</td><td>AZ</td></tr>
	<tr><td> 2286357</td><td> 2350725</td><td> 2673400</td><td> 2915918</td><td> 3011524</td><td>AR</td></tr>
	<tr><td>23667764</td><td>29760021</td><td>33871648</td><td>37253956</td><td>39538223</td><td>CA</td></tr>
	<tr><td> 2889735</td><td> 3294394</td><td> 4301261</td><td> 5029196</td><td> 5773714</td><td>CO</td></tr>
	<tr><td> 3107564</td><td> 3287116</td><td> 3405565</td><td> 3574097</td><td> 3605944</td><td>CT</td></tr>
	<tr><td>  594338</td><td>  666168</td><td>  783600</td><td>  897934</td><td>  989948</td><td>DE</td></tr>
	<tr><td>  638432</td><td>  606900</td><td>  572059</td><td>  601723</td><td>  689545</td><td>DC</td></tr>
	<tr><td> 9746961</td><td>12937926</td><td>15982378</td><td>18801310</td><td>21538187</td><td>FL</td></tr>
	<tr><td> 5462982</td><td> 6478216</td><td> 8186453</td><td> 9687653</td><td>10711908</td><td>GA</td></tr>
	<tr><td>  964691</td><td> 1108229</td><td> 1211537</td><td> 1360301</td><td> 1455271</td><td>HI</td></tr>
	<tr><td>  944127</td><td> 1006749</td><td> 1293953</td><td> 1567582</td><td> 1839106</td><td>ID</td></tr>
	<tr><td>11427409</td><td>11430602</td><td>12419293</td><td>12830632</td><td>12812508</td><td>IL</td></tr>
	<tr><td> 5490210</td><td> 5544159</td><td> 6080485</td><td> 6483802</td><td> 6785528</td><td>IN</td></tr>
	<tr><td> 2913808</td><td> 2776755</td><td> 2926324</td><td> 3046355</td><td> 3190369</td><td>IA</td></tr>
	<tr><td> 2364236</td><td> 2477574</td><td> 2688418</td><td> 2853118</td><td> 2937880</td><td>KS</td></tr>
	<tr><td> 3660324</td><td> 3685296</td><td> 4041769</td><td> 4339367</td><td> 4505836</td><td>KY</td></tr>
	<tr><td> 4206116</td><td> 4219973</td><td> 4468976</td><td> 4533372</td><td> 4657757</td><td>LA</td></tr>
	<tr><td> 1125043</td><td> 1227928</td><td> 1274923</td><td> 1328361</td><td> 1362359</td><td>ME</td></tr>
	<tr><td> 4216933</td><td> 4781468</td><td> 5296486</td><td> 5773552</td><td> 6177224</td><td>MD</td></tr>
	<tr><td> 5737093</td><td> 6016425</td><td> 6349097</td><td> 6547629</td><td> 7029917</td><td>MA</td></tr>
	<tr><td> 9262044</td><td> 9295297</td><td> 9938444</td><td> 9883640</td><td>10077331</td><td>MI</td></tr>
	<tr><td> 4075970</td><td> 4375099</td><td> 4919479</td><td> 5303925</td><td> 5706494</td><td>MN</td></tr>
	<tr><td> 2520770</td><td> 2573216</td><td> 2844658</td><td> 2967297</td><td> 2961279</td><td>MS</td></tr>
	<tr><td> 4916766</td><td> 5117073</td><td> 5595211</td><td> 5988927</td><td> 6154913</td><td>MO</td></tr>
	<tr><td>  786690</td><td>  799065</td><td>  902195</td><td>  989415</td><td> 1084225</td><td>MT</td></tr>
	<tr><td> 1569825</td><td> 1578385</td><td> 1711263</td><td> 1826341</td><td> 1961504</td><td>NE</td></tr>
	<tr><td>  800508</td><td> 1201833</td><td> 1998257</td><td> 2700551</td><td> 3104614</td><td>NV</td></tr>
	<tr><td>  920610</td><td> 1109252</td><td> 1235786</td><td> 1316470</td><td> 1377529</td><td>NH</td></tr>
	<tr><td> 7365011</td><td> 7730188</td><td> 8414350</td><td> 8791894</td><td> 9288994</td><td>NJ</td></tr>
	<tr><td> 1303302</td><td> 1515069</td><td> 1819046</td><td> 2059179</td><td> 2117522</td><td>NM</td></tr>
	<tr><td>17558165</td><td>17990455</td><td>18976457</td><td>19378102</td><td>20201249</td><td>NY</td></tr>
	<tr><td> 5880095</td><td> 6628637</td><td> 8049313</td><td> 9535483</td><td>10439388</td><td>NC</td></tr>
	<tr><td>  652717</td><td>  638800</td><td>  642200</td><td>  672591</td><td>  779094</td><td>ND</td></tr>
	<tr><td>10797603</td><td>10847115</td><td>11353140</td><td>11536504</td><td>11799448</td><td>OH</td></tr>
	<tr><td> 3025487</td><td> 3145585</td><td> 3450654</td><td> 3751351</td><td> 3959353</td><td>OK</td></tr>
	<tr><td> 2633156</td><td> 2842321</td><td> 3421399</td><td> 3831074</td><td> 4237256</td><td>OR</td></tr>
	<tr><td>11864720</td><td>11881643</td><td>12281054</td><td>12702379</td><td>13002700</td><td>PA</td></tr>
	<tr><td>  947154</td><td> 1003464</td><td> 1048319</td><td> 1052567</td><td> 1097379</td><td>RI</td></tr>
	<tr><td> 3120729</td><td> 3486703</td><td> 4012012</td><td> 4625364</td><td> 5118425</td><td>SC</td></tr>
	<tr><td>  690768</td><td>  696004</td><td>  754844</td><td>  814180</td><td>  886667</td><td>SD</td></tr>
	<tr><td> 4591023</td><td> 4877185</td><td> 5689283</td><td> 6346105</td><td> 6910840</td><td>TN</td></tr>
	<tr><td>14255513</td><td>16986510</td><td>20851820</td><td>25145561</td><td>29145505</td><td>TX</td></tr>
	<tr><td> 1461037</td><td> 1722850</td><td> 2233169</td><td> 2763885</td><td> 3271616</td><td>UT</td></tr>
	<tr><td>  511456</td><td>  562758</td><td>  608827</td><td>  625741</td><td>  643077</td><td>VT</td></tr>
	<tr><td> 5346797</td><td> 6187358</td><td> 7078515</td><td> 8001024</td><td> 8631393</td><td>VA</td></tr>
	<tr><td> 4132353</td><td> 4866692</td><td> 5894121</td><td> 6724540</td><td> 7705281</td><td>WA</td></tr>
	<tr><td> 1950186</td><td> 1793477</td><td> 1808344</td><td> 1852994</td><td> 1793716</td><td>WV</td></tr>
	<tr><td> 4705642</td><td> 4891769</td><td> 5363675</td><td> 5686986</td><td> 5893718</td><td>WI</td></tr>
	<tr><td>  469557</td><td>  453588</td><td>  493782</td><td>  563626</td><td>  576851</td><td>WY</td></tr>
</tbody>
</table>




```R
pop <- gather(pop, key = "Year", value = "population", 1:5)   


```


```R
pop$Year <- as.Date(pop$Year, format = "%Y")


```


```R

setnames(pop, 'Abbreviation', 'State') 
pop$State <- as.factor(pop$State)

```


```R
ggplot(pop, aes(x= Year, y = population)) +
geom_line() +
facet_wrap(~State, scales = "free") + 
theme(
  axis.text.x = element_blank())+
labs()

```


    

    



```R
#These data wouldn't affect the first question, because we want to find decreasing in the number of murders
#But it can help us to see a wider picture of what is going on 
#As we have decreasing in population in some states
```


```R


```


```R
#Analyzing murder statistics, we can choose a few states which were really good in decreasing this level 
#It will be CA, CT, HI, MI, NJS, NY, TX, WY, DC, WV
good_crime_data <- crime_data %>%
filter(State %in% c("CA", "CT",  "HI", "MI", "NJS", "NY",  "TX", "WY", "WV", "DC"))


ggplot(good_crime_data, aes(x= Year, y = n)) + 
geom_line() + 
facet_wrap(vars(State), scales = "free") + 
labs(y = "Number  of murders", x = "", titile = "Trend of number of murders")

good_pop <- pop %>%
filter(State %in% c("CA", "CT",  "HI", "MI", "NJS", "NY",  "TX", "WY", "WV", "DC"))
ggplot(good_pop, aes(x= Year, y = population)) +
geom_line() +
facet_wrap(~State, scales = "free") + 
scale_y_log10()+
labs(title = "Popilation")


```


    
![png](YearVsNumbeerOfCrimes.png)
    



    
![png](top9 state in cutting murders.png)
    



```R
#Here we see more visible graphs for states that were better in decreasing the number of  murders, and most of them increased
#the population, otherwise WV and DC didn't
```


```R
crime_data_max_good <- crime_data_max %>%
filter((State %in% c("CA", "CT",  "HI", "MI", "NJS", "NY",  "TX", "WY", "WV", "DC")))

```


```R
crime_data_max_good
```


<table class="dataframe">
<caption>A tibble: 9 × 3</caption>
<thead>
	<tr><th scope=col>State</th><th scope=col>max</th><th scope=col>Year</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;date&gt;</th></tr>
</thead>
<tbody>
	<tr><td>CA</td><td>4370</td><td>1993-07-04</td></tr>
	<tr><td>CT</td><td> 222</td><td>1993-07-04</td></tr>
	<tr><td>DC</td><td> 489</td><td>1991-07-04</td></tr>
	<tr><td>HI</td><td>  84</td><td>1980-07-04</td></tr>
	<tr><td>MI</td><td>1153</td><td>1987-07-04</td></tr>
	<tr><td>NY</td><td>2511</td><td>1993-07-04</td></tr>
	<tr><td>TX</td><td>2768</td><td>1991-07-04</td></tr>
	<tr><td>WV</td><td> 134</td><td>1980-07-04</td></tr>
	<tr><td>WY</td><td>  44</td><td>1979-07-04</td></tr>
</tbody>
</table>




```R
#Now we know when these states hit the highest number of murders in the last 40 years

```


```R
#To analyze the rate of murders per population, I'm going to build graphs that wouldn't be very accurate, but 
#They would be able to show us the trends

#As I don't have population data for each year I'm going to create a table for the 1980, 1990, 2000, and 2010 years
```


```R

```


<table class="dataframe">
<caption>A tibble: 253 × 5</caption>
<thead>
	<tr><th scope=col>State</th><th scope=col>Year</th><th scope=col>n</th><th scope=col>population</th><th scope=col>murder_rate_100k</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;date&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>AK</td><td>1980-07-04</td><td>  47</td><td>  401851</td><td>11.6958773</td></tr>
	<tr><td>AK</td><td>1990-07-04</td><td>  37</td><td>  550043</td><td> 6.7267468</td></tr>
	<tr><td>AK</td><td>2000-07-04</td><td>  35</td><td>  626932</td><td> 5.5827426</td></tr>
	<tr><td>AK</td><td>2010-07-04</td><td>  43</td><td>  710231</td><td> 6.0543682</td></tr>
	<tr><td>AK</td><td>2020-07-04</td><td>  56</td><td>  733391</td><td> 7.6357632</td></tr>
	<tr><td>AL</td><td>1980-07-04</td><td> 513</td><td> 3894025</td><td>13.1740294</td></tr>
	<tr><td>AL</td><td>1990-07-04</td><td> 463</td><td> 4040587</td><td>11.4587311</td></tr>
	<tr><td>AL</td><td>2000-07-04</td><td> 235</td><td> 4447100</td><td> 5.2843426</td></tr>
	<tr><td>AL</td><td>2010-07-04</td><td> 191</td><td> 4779736</td><td> 3.9960366</td></tr>
	<tr><td>AL</td><td>2020-07-04</td><td>  24</td><td> 5024279</td><td> 0.4776805</td></tr>
	<tr><td>AR</td><td>1980-07-04</td><td> 214</td><td> 2286357</td><td> 9.3598681</td></tr>
	<tr><td>AR</td><td>1990-07-04</td><td> 254</td><td> 2350725</td><td>10.8051771</td></tr>
	<tr><td>AR</td><td>2000-07-04</td><td> 163</td><td> 2673400</td><td> 6.0971048</td></tr>
	<tr><td>AR</td><td>2010-07-04</td><td> 141</td><td> 2915918</td><td> 4.8355269</td></tr>
	<tr><td>AR</td><td>2020-07-04</td><td> 340</td><td> 3011524</td><td>11.2899648</td></tr>
	<tr><td>AZ</td><td>1980-07-04</td><td> 293</td><td> 2716546</td><td>10.7857551</td></tr>
	<tr><td>AZ</td><td>1990-07-04</td><td> 301</td><td> 3665228</td><td> 8.2123131</td></tr>
	<tr><td>AZ</td><td>2000-07-04</td><td> 385</td><td> 5130632</td><td> 7.5039488</td></tr>
	<tr><td>AZ</td><td>2010-07-04</td><td> 400</td><td> 6392017</td><td> 6.2578056</td></tr>
	<tr><td>AZ</td><td>2020-07-04</td><td> 523</td><td> 7151502</td><td> 7.3131490</td></tr>
	<tr><td>CA</td><td>1980-07-04</td><td>3590</td><td>23667764</td><td>15.1683108</td></tr>
	<tr><td>CA</td><td>1990-07-04</td><td>3862</td><td>29760021</td><td>12.9771414</td></tr>
	<tr><td>CA</td><td>2000-07-04</td><td>2219</td><td>33871648</td><td> 6.5512018</td></tr>
	<tr><td>CA</td><td>2010-07-04</td><td>1948</td><td>37253956</td><td> 5.2289749</td></tr>
	<tr><td>CA</td><td>2020-07-04</td><td>2379</td><td>39538223</td><td> 6.0169624</td></tr>
	<tr><td>CO</td><td>1980-07-04</td><td> 198</td><td> 2889735</td><td> 6.8518394</td></tr>
	<tr><td>CO</td><td>1990-07-04</td><td> 166</td><td> 3294394</td><td> 5.0388630</td></tr>
	<tr><td>CO</td><td>2000-07-04</td><td> 134</td><td> 4301261</td><td> 3.1153655</td></tr>
	<tr><td>CO</td><td>2010-07-04</td><td> 145</td><td> 5029196</td><td> 2.8831646</td></tr>
	<tr><td>CO</td><td>2020-07-04</td><td> 349</td><td> 5773714</td><td> 6.0446361</td></tr>
	<tr><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td></tr>
	<tr><td>VA</td><td>1980-07-04</td><td>468</td><td>5346797</td><td>8.752904</td></tr>
	<tr><td>VA</td><td>1990-07-04</td><td>561</td><td>6187358</td><td>9.066875</td></tr>
	<tr><td>VA</td><td>2000-07-04</td><td>398</td><td>7078515</td><td>5.622648</td></tr>
	<tr><td>VA</td><td>2010-07-04</td><td>387</td><td>8001024</td><td>4.836881</td></tr>
	<tr><td>VA</td><td>2020-07-04</td><td>585</td><td>8631393</td><td>6.777585</td></tr>
	<tr><td>VT</td><td>1980-07-04</td><td>  9</td><td> 511456</td><td>1.759682</td></tr>
	<tr><td>VT</td><td>1990-07-04</td><td> 14</td><td> 562758</td><td>2.487748</td></tr>
	<tr><td>VT</td><td>2000-07-04</td><td> 10</td><td> 608827</td><td>1.642503</td></tr>
	<tr><td>VT</td><td>2010-07-04</td><td>  7</td><td> 625741</td><td>1.118674</td></tr>
	<tr><td>VT</td><td>2020-07-04</td><td> 18</td><td> 643077</td><td>2.799043</td></tr>
	<tr><td>WA</td><td>1980-07-04</td><td>222</td><td>4132353</td><td>5.372242</td></tr>
	<tr><td>WA</td><td>1990-07-04</td><td>249</td><td>4866692</td><td>5.116412</td></tr>
	<tr><td>WA</td><td>2000-07-04</td><td>203</td><td>5894121</td><td>3.444110</td></tr>
	<tr><td>WA</td><td>2010-07-04</td><td>175</td><td>6724540</td><td>2.602408</td></tr>
	<tr><td>WA</td><td>2020-07-04</td><td>337</td><td>7705281</td><td>4.373624</td></tr>
	<tr><td>WI</td><td>1980-07-04</td><td>145</td><td>4705642</td><td>3.081407</td></tr>
	<tr><td>WI</td><td>1990-07-04</td><td>238</td><td>4891769</td><td>4.865316</td></tr>
	<tr><td>WI</td><td>2000-07-04</td><td>172</td><td>5363675</td><td>3.206757</td></tr>
	<tr><td>WI</td><td>2010-07-04</td><td>170</td><td>5686986</td><td>2.989281</td></tr>
	<tr><td>WI</td><td>2020-07-04</td><td>423</td><td>5893718</td><td>7.177133</td></tr>
	<tr><td>WV</td><td>1980-07-04</td><td>134</td><td>1950186</td><td>6.871139</td></tr>
	<tr><td>WV</td><td>1990-07-04</td><td>110</td><td>1793477</td><td>6.133338</td></tr>
	<tr><td>WV</td><td>2000-07-04</td><td> 64</td><td>1808344</td><td>3.539150</td></tr>
	<tr><td>WV</td><td>2010-07-04</td><td> 57</td><td>1852994</td><td>3.076103</td></tr>
	<tr><td>WV</td><td>2020-07-04</td><td>120</td><td>1793716</td><td>6.690022</td></tr>
	<tr><td>WY</td><td>1980-07-04</td><td> 34</td><td> 469557</td><td>7.240867</td></tr>
	<tr><td>WY</td><td>1990-07-04</td><td> 23</td><td> 453588</td><td>5.070681</td></tr>
	<tr><td>WY</td><td>2000-07-04</td><td> 12</td><td> 493782</td><td>2.430222</td></tr>
	<tr><td>WY</td><td>2010-07-04</td><td>  8</td><td> 563626</td><td>1.419381</td></tr>
	<tr><td>WY</td><td>2020-07-04</td><td> 20</td><td> 576851</td><td>3.467100</td></tr>
</tbody>
</table>




```R
 murder_rate_table <- crime_data %>%
inner_join(pop, by = c("State", "Year")) %>%
mutate(murder_rate_100k = n / population * 100000)

```


```R
ggplot(murder_rate_table, aes(x= Year, y = murder_rate_100k)) + 
geom_line() + 
facet_wrap(vars(State)) + 
labs(title = "Murders rate", y = "Number  of murders per 100k", x = "", titile = "Trend of murders rate") + 
theme(
  axis.text.x = element_blank())


```


    
![png](numberOfmurdersPer100k.png)
    



```R
#In the early 1990s, Washington, D.C., was known as the nation's "murder capital" 
#The elevated crime levels were associated with 
#the introduction of crack cocaine during the late 1980s and early 1990s.


#So comparing all states on the scale isn't informative, so that I will deduct DC

murder_rate_table_50 <- murder_rate_table %>%
filter(State != "DC")

```


```R
ggplot(murder_rate_table_50, aes(x= Year, y = murder_rate_100k)) + 
geom_line() + 
facet_wrap(vars(State)) + 
labs(title = "Murders rate without DC", y = "Number  of murders per 100k", x = "", titile = "Trend of murders rate") + 
theme(
  axis.text.x = element_blank())


```


    
![png](murderRateWithoutDc.png)
    



```R
#From these graphs we can find states, that were decreasing their level of murders better, than others:
# AL, CA, FL, HI, NV, NY, TX 

#As we can they're differences between states with decreasing number of murders and level of murders
```


```R
#Also we can make the coclusion that the safest states are:
#ID, MA, ME, MN, NE, NH, RI, VT, UT
```


```R
murder_rate_table %>%
filter(format(Year, format = "%Y") == "2010") %>%
arrange(murder_rate_100k) %>%
head(3)


murder_rate_table %>%
filter(format(Year, format = "%Y") == "2010") %>%
arrange(desc(murder_rate_100k)) %>%
head(3)
```


<table class="dataframe">
<caption>A tibble: 3 × 5</caption>
<thead>
	<tr><th scope=col>State</th><th scope=col>Year</th><th scope=col>n</th><th scope=col>population</th><th scope=col>murder_rate_100k</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;date&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>NH</td><td>2010-07-04</td><td>13</td><td>1316470</td><td>0.9874893</td></tr>
	<tr><td>VT</td><td>2010-07-04</td><td> 7</td><td> 625741</td><td>1.1186737</td></tr>
	<tr><td>IA</td><td>2010-07-04</td><td>39</td><td>3046355</td><td>1.2802185</td></tr>
</tbody>
</table>




<table class="dataframe">
<caption>A tibble: 3 × 5</caption>
<thead>
	<tr><th scope=col>State</th><th scope=col>Year</th><th scope=col>n</th><th scope=col>population</th><th scope=col>murder_rate_100k</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;date&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>DC</td><td>2010-07-04</td><td>134</td><td> 601723</td><td>22.26938</td></tr>
	<tr><td>LA</td><td>2010-07-04</td><td>493</td><td>4533372</td><td>10.87491</td></tr>
	<tr><td>MD</td><td>2010-07-04</td><td>445</td><td>5773552</td><td> 7.70756</td></tr>
</tbody>
</table>




```R
#I'm taking top 3 states with the lowest and the highest  murder rates
#Iowa has an index
```


```R
gun_table <- read_excel("/Users/daniel/Desktop/Projects/sources_crime/2011_gunlaw_data.xls", na = " ",col_types = c("text", "text"), col_names = c("State", "Index")) 
gun_table$Index <- as.numeric(gun_table$Index)
gun_table <- gun_table %>%
filter(str_detect(State, "United States"))
#Deleating the federal stats
gun_table <- gun_table[-1, ]
 

gun_table$State <- str_remove_all(gun_table$State, pattern = "United States")
gun_table$State <- str_remove_all(gun_table$State, '\\W')
```

    Warning message in eval(expr, envir, enclos):
    “NAs introduced by coercion”



```R

```


```R

```


<table class="dataframe">
<caption>A tibble: 53 × 2</caption>
<thead>
	<tr><th scope=col>State</th><th scope=col>Index</th></tr>
	<tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>Alabama           </td><td>6.80</td></tr>
	<tr><td>Alaska            </td><td>8.00</td></tr>
	<tr><td>Arizona           </td><td>9.33</td></tr>
	<tr><td>Arkansas          </td><td>6.20</td></tr>
	<tr><td>California        </td><td>4.85</td></tr>
	<tr><td>Colorado          </td><td>7.65</td></tr>
	<tr><td>Connecticut       </td><td>5.10</td></tr>
	<tr><td>Delaware          </td><td>6.30</td></tr>
	<tr><td>DistrictofColumbia</td><td>2.00</td></tr>
	<tr><td>Florida           </td><td>6.45</td></tr>
	<tr><td>Georgia           </td><td>7.05</td></tr>
	<tr><td>Guam              </td><td>2.60</td></tr>
	<tr><td>Hawaii            </td><td>2.60</td></tr>
	<tr><td>Idaho             </td><td>8.73</td></tr>
	<tr><td>Illinois          </td><td>3.00</td></tr>
	<tr><td>Indiana           </td><td>6.80</td></tr>
	<tr><td>Iowa              </td><td>5.80</td></tr>
	<tr><td>Kansas            </td><td>7.40</td></tr>
	<tr><td>Kentucky          </td><td>7.40</td></tr>
	<tr><td>Louisiana         </td><td>6.90</td></tr>
	<tr><td>Maine             </td><td>7.40</td></tr>
	<tr><td>Maryland          </td><td>5.10</td></tr>
	<tr><td>Massachusetts     </td><td>3.70</td></tr>
	<tr><td>Michigan          </td><td>6.30</td></tr>
	<tr><td>Minnesota         </td><td>6.05</td></tr>
	<tr><td>Mississippi       </td><td>7.40</td></tr>
	<tr><td>Missouri          </td><td>7.40</td></tr>
	<tr><td>Montana           </td><td>8.73</td></tr>
	<tr><td>Nebraska          </td><td>7.40</td></tr>
	<tr><td>Nevada            </td><td>7.40</td></tr>
	<tr><td>NewHampshire      </td><td>7.40</td></tr>
	<tr><td>NewJersey         </td><td>4.10</td></tr>
	<tr><td>NewMexico         </td><td>7.40</td></tr>
	<tr><td>NewYork           </td><td>4.60</td></tr>
	<tr><td>NorthCarolina     </td><td>6.90</td></tr>
	<tr><td>NorthDakota       </td><td>6.20</td></tr>
	<tr><td>Ohio              </td><td>7.40</td></tr>
	<tr><td>Oklahoma          </td><td>6.20</td></tr>
	<tr><td>Oregon            </td><td>7.40</td></tr>
	<tr><td>Pennsylvania      </td><td>7.40</td></tr>
	<tr><td>PuertoRico        </td><td>3.10</td></tr>
	<tr><td>RhodeIsland       </td><td>4.60</td></tr>
	<tr><td>SouthCarolina     </td><td>6.30</td></tr>
	<tr><td>SouthDakota       </td><td>8.73</td></tr>
	<tr><td>Tennessee         </td><td>8.13</td></tr>
	<tr><td>Texas             </td><td>6.80</td></tr>
	<tr><td>Utah              </td><td>7.53</td></tr>
	<tr><td>Vermont           </td><td>8.00</td></tr>
	<tr><td>Virginia          </td><td>6.90</td></tr>
	<tr><td>Washington        </td><td>6.65</td></tr>
	<tr><td>WestVirginia      </td><td>7.40</td></tr>
	<tr><td>Wisconsin         </td><td>6.20</td></tr>
	<tr><td>Wyoming           </td><td>8.73</td></tr>
</tbody>
</table>




```R
murder_rate_table %>%
filter(format(Year, format = "%Y") == "2010")
```


<table class="dataframe">
<caption>A tibble: 51 × 5</caption>
<thead>
	<tr><th scope=col>State</th><th scope=col>Year</th><th scope=col>n</th><th scope=col>population</th><th scope=col>murder_rate_100k</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;date&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>AK</td><td>2010-07-04</td><td>  43</td><td>  710231</td><td> 6.0543682</td></tr>
	<tr><td>AL</td><td>2010-07-04</td><td> 191</td><td> 4779736</td><td> 3.9960366</td></tr>
	<tr><td>AR</td><td>2010-07-04</td><td> 141</td><td> 2915918</td><td> 4.8355269</td></tr>
	<tr><td>AZ</td><td>2010-07-04</td><td> 400</td><td> 6392017</td><td> 6.2578056</td></tr>
	<tr><td>CA</td><td>2010-07-04</td><td>1948</td><td>37253956</td><td> 5.2289749</td></tr>
	<tr><td>CO</td><td>2010-07-04</td><td> 145</td><td> 5029196</td><td> 2.8831646</td></tr>
	<tr><td>CT</td><td>2010-07-04</td><td> 134</td><td> 3574097</td><td> 3.7491987</td></tr>
	<tr><td>DC</td><td>2010-07-04</td><td> 134</td><td>  601723</td><td>22.2693831</td></tr>
	<tr><td>DE</td><td>2010-07-04</td><td>  52</td><td>  897934</td><td> 5.7910715</td></tr>
	<tr><td>FL</td><td>2010-07-04</td><td>1130</td><td>18801310</td><td> 6.0102195</td></tr>
	<tr><td>GA</td><td>2010-07-04</td><td> 565</td><td> 9687653</td><td> 5.8321660</td></tr>
	<tr><td>HI</td><td>2010-07-04</td><td>  27</td><td> 1360301</td><td> 1.9848548</td></tr>
	<tr><td>IA</td><td>2010-07-04</td><td>  39</td><td> 3046355</td><td> 1.2802185</td></tr>
	<tr><td>ID</td><td>2010-07-04</td><td>  22</td><td> 1567582</td><td> 1.4034354</td></tr>
	<tr><td>IL</td><td>2010-07-04</td><td> 484</td><td>12830632</td><td> 3.7722226</td></tr>
	<tr><td>IN</td><td>2010-07-04</td><td> 291</td><td> 6483802</td><td> 4.4881074</td></tr>
	<tr><td>KS</td><td>2010-07-04</td><td> 107</td><td> 2853118</td><td> 3.7502830</td></tr>
	<tr><td>KY</td><td>2010-07-04</td><td> 189</td><td> 4339367</td><td> 4.3554740</td></tr>
	<tr><td>LA</td><td>2010-07-04</td><td> 493</td><td> 4533372</td><td>10.8749072</td></tr>
	<tr><td>MA</td><td>2010-07-04</td><td> 219</td><td> 6547629</td><td> 3.3447222</td></tr>
	<tr><td>MD</td><td>2010-07-04</td><td> 445</td><td> 5773552</td><td> 7.7075603</td></tr>
	<tr><td>ME</td><td>2010-07-04</td><td>  27</td><td> 1328361</td><td> 2.0325800</td></tr>
	<tr><td>MI</td><td>2010-07-04</td><td> 606</td><td> 9883640</td><td> 6.1313443</td></tr>
	<tr><td>MN</td><td>2010-07-04</td><td> 105</td><td> 5303925</td><td> 1.9796660</td></tr>
	<tr><td>MO</td><td>2010-07-04</td><td> 450</td><td> 5988927</td><td> 7.5138668</td></tr>
	<tr><td>MS</td><td>2010-07-04</td><td> 174</td><td> 2967297</td><td> 5.8639226</td></tr>
	<tr><td>MT</td><td>2010-07-04</td><td>  23</td><td>  989415</td><td> 2.3246060</td></tr>
	<tr><td>NC</td><td>2010-07-04</td><td> 468</td><td> 9535483</td><td> 4.9079842</td></tr>
	<tr><td>ND</td><td>2010-07-04</td><td>   9</td><td>  672591</td><td> 1.3381089</td></tr>
	<tr><td>NE</td><td>2010-07-04</td><td>  58</td><td> 1826341</td><td> 3.1757487</td></tr>
	<tr><td>NH</td><td>2010-07-04</td><td>  13</td><td> 1316470</td><td> 0.9874893</td></tr>
	<tr><td>NJ</td><td>2010-07-04</td><td> 392</td><td> 8791894</td><td> 4.4586525</td></tr>
	<tr><td>NM</td><td>2010-07-04</td><td> 142</td><td> 2059179</td><td> 6.8959522</td></tr>
	<tr><td>NV</td><td>2010-07-04</td><td> 175</td><td> 2700551</td><td> 6.4801590</td></tr>
	<tr><td>NY</td><td>2010-07-04</td><td> 863</td><td>19378102</td><td> 4.4534805</td></tr>
	<tr><td>OH</td><td>2010-07-04</td><td> 481</td><td>11536504</td><td> 4.1693740</td></tr>
	<tr><td>OK</td><td>2010-07-04</td><td> 209</td><td> 3751351</td><td> 5.5713262</td></tr>
	<tr><td>OR</td><td>2010-07-04</td><td>  93</td><td> 3831074</td><td> 2.4275177</td></tr>
	<tr><td>PA</td><td>2010-07-04</td><td> 699</td><td>12702379</td><td> 5.5029062</td></tr>
	<tr><td>RI</td><td>2010-07-04</td><td>  30</td><td> 1052567</td><td> 2.8501749</td></tr>
	<tr><td>SC</td><td>2010-07-04</td><td> 278</td><td> 4625364</td><td> 6.0103378</td></tr>
	<tr><td>SD</td><td>2010-07-04</td><td>  17</td><td>  814180</td><td> 2.0879904</td></tr>
	<tr><td>TN</td><td>2010-07-04</td><td> 397</td><td> 6346105</td><td> 6.2558057</td></tr>
	<tr><td>TX</td><td>2010-07-04</td><td>1363</td><td>25145561</td><td> 5.4204398</td></tr>
	<tr><td>UT</td><td>2010-07-04</td><td>  58</td><td> 2763885</td><td> 2.0984954</td></tr>
	<tr><td>VA</td><td>2010-07-04</td><td> 387</td><td> 8001024</td><td> 4.8368809</td></tr>
	<tr><td>VT</td><td>2010-07-04</td><td>   7</td><td>  625741</td><td> 1.1186737</td></tr>
	<tr><td>WA</td><td>2010-07-04</td><td> 175</td><td> 6724540</td><td> 2.6024085</td></tr>
	<tr><td>WI</td><td>2010-07-04</td><td> 170</td><td> 5686986</td><td> 2.9892811</td></tr>
	<tr><td>WV</td><td>2010-07-04</td><td>  57</td><td> 1852994</td><td> 3.0761028</td></tr>
	<tr><td>WY</td><td>2010-07-04</td><td>   8</td><td>  563626</td><td> 1.4193809</td></tr>
</tbody>
</table>




```R
gun_table <- gun_table %>%
left_join(state_code, by = c("State" = "Name")) %>%
select(4, 2) %>%
rename('State' = 'Abbreviation')
```


```R
murder_vs_gun <- murder_rate_table %>%
filter(format(Year, format = "%Y") == "2010") %>%
inner_join(gun_table, by = "State") %>%
select(State, murder_rate_100k, Index)
```


```R
print(upper_bound <- quantile(murder_vs_gun$Index, 0.75))

print(lower_bound <- quantile(murder_vs_gun$Index, 0.25))
max(murder_vs_gun$Index)

min(murder_vs_gun$Index)

#Based on this numbers I'm going to split data into 3 categories
```

    75% 
    7.4 
    25% 
    6.2 



9.33



2



```R
murder_vs_gun <- murder_vs_gun %>%
mutate(Index_class = ifelse(Index <= lower_bound, "Anti-gun", 
                            ifelse(Index < upper_bound, "Middle", 
                            ifelse(Index >= upper_bound, "Pro-gun", "NA"))))

murder_vs_gun$Index_class <- as.factor(murder_vs_gun$Index_class)
```


```R
ggplot(murder_vs_gun, aes(x = Index, y = murder_rate_100k, label = State, color = Index_class)) + 
geom_point() + 
geom_label() + 
scale_color_manual(values = c("Anti-gun" = 'red', "Middle" = 'orange', "Pro-gun" = 'Green'))+ 
scale_y_log10() 
```


    
![png](murder_RateVsGunIndex.png)
    



```R
cor(murder_vs_gun$Index, murder_vs_gun$murder_rate_100k)

#As we can see from the calculation and the graph, there's no correlation between gun freedom
#and murder rate

```


-0.327823087058607

murder_vs_gun

```R
murder_vs_gun

```


<table class="dataframe">
<caption>A tibble: 51 × 4</caption>
<thead>
	<tr><th scope=col>State</th><th scope=col>murder_rate_100k</th><th scope=col>Index</th><th scope=col>Index_class</th></tr>
	<tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;fct&gt;</th></tr>
</thead>
<tbody>
	<tr><td>AK</td><td> 6.0543682</td><td>8.00</td><td>Pro-gun </td></tr>
	<tr><td>AL</td><td> 3.9960366</td><td>6.80</td><td>Middle  </td></tr>
	<tr><td>AR</td><td> 4.8355269</td><td>6.20</td><td>Anti-gun</td></tr>
	<tr><td>AZ</td><td> 6.2578056</td><td>9.33</td><td>Pro-gun </td></tr>
	<tr><td>CA</td><td> 5.2289749</td><td>4.85</td><td>Anti-gun</td></tr>
	<tr><td>CO</td><td> 2.8831646</td><td>7.65</td><td>Pro-gun </td></tr>
	<tr><td>CT</td><td> 3.7491987</td><td>5.10</td><td>Anti-gun</td></tr>
	<tr><td>DC</td><td>22.2693831</td><td>2.00</td><td>Anti-gun</td></tr>
	<tr><td>DE</td><td> 5.7910715</td><td>6.30</td><td>Middle  </td></tr>
	<tr><td>FL</td><td> 6.0102195</td><td>6.45</td><td>Middle  </td></tr>
	<tr><td>GA</td><td> 5.8321660</td><td>7.05</td><td>Middle  </td></tr>
	<tr><td>HI</td><td> 1.9848548</td><td>2.60</td><td>Anti-gun</td></tr>
	<tr><td>IA</td><td> 1.2802185</td><td>5.80</td><td>Anti-gun</td></tr>
	<tr><td>ID</td><td> 1.4034354</td><td>8.73</td><td>Pro-gun </td></tr>
	<tr><td>IL</td><td> 3.7722226</td><td>3.00</td><td>Anti-gun</td></tr>
	<tr><td>IN</td><td> 4.4881074</td><td>6.80</td><td>Middle  </td></tr>
	<tr><td>KS</td><td> 3.7502830</td><td>7.40</td><td>Pro-gun </td></tr>
	<tr><td>KY</td><td> 4.3554740</td><td>7.40</td><td>Pro-gun </td></tr>
	<tr><td>LA</td><td>10.8749072</td><td>6.90</td><td>Middle  </td></tr>
	<tr><td>MA</td><td> 3.3447222</td><td>3.70</td><td>Anti-gun</td></tr>
	<tr><td>MD</td><td> 7.7075603</td><td>5.10</td><td>Anti-gun</td></tr>
	<tr><td>ME</td><td> 2.0325800</td><td>7.40</td><td>Pro-gun </td></tr>
	<tr><td>MI</td><td> 6.1313443</td><td>6.30</td><td>Middle  </td></tr>
	<tr><td>MN</td><td> 1.9796660</td><td>6.05</td><td>Anti-gun</td></tr>
	<tr><td>MO</td><td> 7.5138668</td><td>7.40</td><td>Pro-gun </td></tr>
	<tr><td>MS</td><td> 5.8639226</td><td>7.40</td><td>Pro-gun </td></tr>
	<tr><td>MT</td><td> 2.3246060</td><td>8.73</td><td>Pro-gun </td></tr>
	<tr><td>NC</td><td> 4.9079842</td><td>6.90</td><td>Middle  </td></tr>
	<tr><td>ND</td><td> 1.3381089</td><td>6.20</td><td>Anti-gun</td></tr>
	<tr><td>NE</td><td> 3.1757487</td><td>7.40</td><td>Pro-gun </td></tr>
	<tr><td>NH</td><td> 0.9874893</td><td>7.40</td><td>Pro-gun </td></tr>
	<tr><td>NJ</td><td> 4.4586525</td><td>4.10</td><td>Anti-gun</td></tr>
	<tr><td>NM</td><td> 6.8959522</td><td>7.40</td><td>Pro-gun </td></tr>
	<tr><td>NV</td><td> 6.4801590</td><td>7.40</td><td>Pro-gun </td></tr>
	<tr><td>NY</td><td> 4.4534805</td><td>4.60</td><td>Anti-gun</td></tr>
	<tr><td>OH</td><td> 4.1693740</td><td>7.40</td><td>Pro-gun </td></tr>
	<tr><td>OK</td><td> 5.5713262</td><td>6.20</td><td>Anti-gun</td></tr>
	<tr><td>OR</td><td> 2.4275177</td><td>7.40</td><td>Pro-gun </td></tr>
	<tr><td>PA</td><td> 5.5029062</td><td>7.40</td><td>Pro-gun </td></tr>
	<tr><td>RI</td><td> 2.8501749</td><td>4.60</td><td>Anti-gun</td></tr>
	<tr><td>SC</td><td> 6.0103378</td><td>6.30</td><td>Middle  </td></tr>
	<tr><td>SD</td><td> 2.0879904</td><td>8.73</td><td>Pro-gun </td></tr>
	<tr><td>TN</td><td> 6.2558057</td><td>8.13</td><td>Pro-gun </td></tr>
	<tr><td>TX</td><td> 5.4204398</td><td>6.80</td><td>Middle  </td></tr>
	<tr><td>UT</td><td> 2.0984954</td><td>7.53</td><td>Pro-gun </td></tr>
	<tr><td>VA</td><td> 4.8368809</td><td>6.90</td><td>Middle  </td></tr>
	<tr><td>VT</td><td> 1.1186737</td><td>8.00</td><td>Pro-gun </td></tr>
	<tr><td>WA</td><td> 2.6024085</td><td>6.65</td><td>Middle  </td></tr>
	<tr><td>WI</td><td> 2.9892811</td><td>6.20</td><td>Anti-gun</td></tr>
	<tr><td>WV</td><td> 3.0761028</td><td>7.40</td><td>Pro-gun </td></tr>
	<tr><td>WY</td><td> 1.4193809</td><td>8.73</td><td>Pro-gun </td></tr>
</tbody>
</table>


