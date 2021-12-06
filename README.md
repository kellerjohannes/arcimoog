# Arcimoog

Tools to create and manage control voltages for analog synthesis through an audio interface.


## Approaches in this collection of tools

To generate control voltages (CV) digitally, we use a DC-coupled audio interface. So far, multichannel interfaces from MOOG were successfully tested. To generate audio signals that will be converted by the audio interface into CV digital tools are required. The following approaches are being tested.

### Reaper plugins

#### Development notes

##### Proof of concept (2nd of December 2021)

A rudimentary reaper plugin was used to generate constant CVs, using a MOTU UltraLite MK4 interface (8 output channels). This setup it functional for the following aspects:

- The CVs are constant, without fluctuations or distortions. Used to modulate the VCO, the pitch is perfectly stable.
- The CVs are reproductible. Used to modulate the VCO, the output values of the plugin lead to the same sounding frequencies.
- The CVs are predictable. Used to modulate the VCO, intervals can be calculated in a straight forward way, for example by using log for ratio based interval definitions. 

#### User Guide: Installation

The Reaper plugins used here are based on the EEL script language, therefore they are interpreted by Reaper without needing any additional dependencies. To make the plugins accessible to Reaper, they need to be copied into the Plugin folder structure of the Reaper installation. The location of Plugins can be obtained using the Menu "Options -> Show Reaper source path ...". 

#### User Guide: Concepts



### PureData patch

### Application
