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

To use the plugins it is recommended to create one track per Moog (per voice). Each track needs to consist of as many output channels as CVs you want to generate. The default assumption of the plugin is 6 channels per track. The plugin doesn't have any inputs. The parameters of the plugin can be manipulated either in real time (by moving the sliders in the plugin interface) or with the Reaper automatisation features. 

In a rudimentary implementation, you can set the output value for each channel directly.

In a more complex implementation, there are the following sliders available:
- for the VCO modulation:
  - selection of a pitch from a predefined scale
  - setting a scaling factor to tune the output
  - setting a shift value to transpose the output
  - setting a detune value to modify the selected, tuned and transposed pitch
- for the gate output:
  - a selection between on and off
- for the VCF cutoff and resonance, and the VCA:
  - selection of a 'sillable' which will define the settings of the output channels for the VCF cutoff, the VCF resonance and the VCA

To change the scale and sillable values it is necessary to adapt the source code of the plugin accordingly. This is possible from within Reaper, in the plugin editor interface. 

If you use git to get the latest versions of the plugins it is recommended to soft link the plugin files from the local git repository into the Reaper effects folder. 

On Linux / macOS:
```
ln -s /path/to/plugin/file/in/git /path/to/reaper/effects/folder
```

This is an example to make the plugin "arcimoog-default" available in Reaper. You need to adapt the paths to your system.

```
ln -s ~/Documents/arcimoog/reaper-plugins/arcimoog-default ~/Library/Application\ Support/REAPER/Effects/arcimoog-default
```


### PureData patch

### Application
