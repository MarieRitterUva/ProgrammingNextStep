## SimulADE - Simulating Addiction with Difference Equations

This is a program that was developed by Marie Ritter on the basis of a paper by Grasman, Grasman, and van der Maas (2016). The program implements the model proposed in this paper and provides a graphical user interface (GUI) to interact with the program. From input parameters the program simulates the progress of addiction and creates graphs over time as well as a bifurcation diagram. Furthermore, the program provides the option to estimate how successful a therapy might be when entering parameters for both the therapy time as well as the time afterwards under the tab _Therapy Success_ (in development).  
Lastly, users can download the produced data file from the last simulation in order to inspect it further.

#### Development Circumstances

This is a program created under supervision from Professor Han van der Maas and Claire Stevenson for the course "Programming: The next step" at the Universiteit van Amsterdam in Spring 2016.

#### Operation Instructions

The program is fairly simple to use. In the _Basic Simulation_ tab the user can choose to simply inspect graphs from the simulation or to customize parameters. A description of all parameters can be found in the _Parameters Explained_ tab. The user is also encouraged to try out different parameters and check what happens - this can be really fun!  
If a user wants to see some more advanced options, he can use the _Bifurcation Diagram_ and/or _Therapy Success_ tab. However, even these tabs are programmed so that users can try out different things and see the results quickly.  
For further instructions, users can always visit the _Instructions_ tab.

#### Files

The complete program includes the following files:

* function.R  
 File from which functions are sourced.
 
* server.R  
 Actual program
 
* ui.R  
 Graphical interface
 
* Readme.md  
 The README file
 
* DESCRIPTION  
 Tags and information for the showcase mode.
 
* Bugs.txt  
 Bug tracking file from developer
 
* www  
 Image files  
 * logo-uva.png

#### Bugs

Known bugs can be seen in the Bugs.txt file.

#### Further information

Further information on the model can be obtained from the paper by Grasman, Grasman, and van der Maas (2016).  
Further information on the program can be obtained fromt the developer: <marie.ritter@student.uva.nl>.

##### Reference

Grasman, J., Grasman, R. P. P. P., & van der Maas (2016). The dynamics of addiction: craving versus self-control. Manuskript submitted for revision.
