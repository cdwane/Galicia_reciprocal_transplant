# Galicia_reciprocal_transplant

#### Author: Christopher Dwane

## **Description of the Data**

This reository contains data and code associated with the manuscript "Divergence in thermal performance contributes to ecotype maintenance in an intertidal snail: evidence from in-situ transplants."

The dataset contains data generated from a field reciprocal transplant experiment conducted at Centinela, Galicia in summer 2022, and an accompanying laboratory experiment conducted simultaneously at ECIMAT- UVIGO using animals collected from the same site.

The objective of the study was to assess physiological differences in cardiac function between "Crab" and "Wave" ecotypes of the intertidal snail, Littorina saxatilis, following 1- and 4-day reciprocal transplantation across shore height within their native habitat, and under controlled laboratory ramps.

Note that the Crab and Wave ecotypes are referred to as "RB" and "SU", respectively in the data and code, reflecting older naming conventions of the two ecotypes in the L. saxatilis literature.)

### **Description of files**

* `field_hr_data.csv`  -> Contains heart rate data from the field reciprocal transplant experiment.

* `field_temperature_data.csv` -> Contains continuous temperature recordings from the field locations used for the reciprocal transplant experiment, recorded throughout the entire period in which the experiment took place.

* `rampingdata.csv` -> Contains heart rate data from the laboratory ramping experiment

* `01. Load raw field data.R`  -> R script to load and clean up `field_hr_data.csv`, and to load and plot the temperature data from `field_temperature_data.csv`. Must be run before script 02.

* `02. Statistical analysis.R` -> R script to run statistical analysis on, and to plot, field heartrate data.

* `03. Laboratory data script.R` -> R script to load, run statistical analysis on, and plot, data from the laboratory ramping experiment saved in the data file `rampingdata.csv`

### **Description of variables within each data file**


 **Variables in** **`field_hr_data.csv`**:

* **unique:** Unique identifier for each heart rate recording
* **day:** Indicates whether the recording was taken 1 day or 4 days after reciprocal transplantation
* **timepoint:** Indicates whether the data represents the first or second recording, within each Day, for that particular individual (two recordings were taken from each indivdual per day)
* **Group:** Refers to which batch the animal was part of, as described in the manuscript. Batch 1 was transplanted on the 12th July, and measurements taken on the 13th and 17th July; Batch 2 was transplanted on the 13th July, and measurements taken on the 14th and 17th July, and Batch 3 was transplanted on the 19th July and measurements taken on the 20th July.
* **Height:** The shore height the animal was transplanted to (either "lower"" (referred to as Mid in the manuscript) or "upper"")
* **Site:** The individual replicate site the animal was transplanted to (three sites per shore height)
* **Input_D1:** Which input channel the animal was recorded on during data collection
* **Input_D4:** Which input channel the animal was recorded on during data collection
* **Population:** Which population the animal was from - "mSU" (Mid-shore Wave), "mRB"(Mid-shore Crab), or "uRB" (Upper shore Crab)
* **Tag:** Number on the tag which the animal was tethered to
* **Colour:** Colour of the tag which the animal was tethered to
* **Size:** Shell height in mm
* **ID:** Unique identifier for each individual snail
* **heartrate:** Heartrate estimate in hz
* **confidence:** Whether the signal represents a clean heartbeat trace ("Good") or should be discarded due to signal noise/ messy signal ("Discard")
* **temperature:** Temperature of the snail immediately prior to the heart rate measurment, estimated using a thermal imaging camera as described in the manuscript
* **SD_hz:** Standard deivation (in hz) of the heartrate signal.

 **Variables in** **`field_temperature_data.csv`**:

* **time:** Timestamp of the temperature recording
* **temp:** Recorded temperature in degrees C (+/- 0.1 degrees C)
* **logger:** Individual identifier for the logger. Number indicates the site at which the logger was placed and the recording was taken - i.e. "upper1A" and "upper1B" were both loggers placed at site "upper1", as referred to in `field_hr_data.csv`
* **height:**  The shore height at which the logger was situated (either "lower"" (referred to as Mid in the manuscript) or "upper"")

 **Variables in** **`rampingdata.csv`**:

* **temp:** Temperature, in degrees C, at which the reording took place, measured using a thermocouple (+/- 0.1 degrees C)
* **heartrate:** Heartrate estimate in hz
* **SD_hz:** Standard deivation (in hz) of the heartrate signal.
* **batch:** Indicates which individual trial the data came from (two seperate trials were run on different days)
* **population:** Which population the animal was from - "SU" (Mid-shore Wave), "mRB"(Mid-shore Crab), or "uRB" (Upper shore Crab)
* **Size:** Shell height in mm
* **snail_id** Unique identifier for each individual snail
* **confidence:** Whether the signal from this individual represents a clean heartbeat trace ("Good") or should be discarded due to signal noise/ messy signal ("Discard")



