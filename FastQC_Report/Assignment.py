
# import required packages
from tkinter import Tk     
from tkinter.filedialog import askopenfilename
import os, os.path
import errno
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np  
import os, sys 
from matplotlib.ticker import (MultipleLocator, FormatStrFormatter)
from tkinter import filedialog

Tk().withdraw()       #get rid of tkinter root window

""" Basic statistics from the given file is extracted in a separate list, the loop iterates from the first line (Basic Statistics)
 to the last line (END_MODULE) and adds the list ta an empty directory."""  
filename = askopenfilename(filetypes=[("Fasta files", "*.txt"), ("All Files", "*.*")]) # Opens file dialog box to choose the desired qc file.
select_path = filedialog.askdirectory()
os.chdir(select_path) #change directory
#retval = os.getcwd() #check current working directory
#print(os.getcwd()) 
Module=("Basic Statistics" "\n")    # module name gets printed with each module information. 
print(filename)
with open(filename, 'r') as file:
    Basic_Statistics=[]              # creates empty list
    
    for line in file:                              #Reads the file line by line and fill the assigned list with information
        if line.startswith(">>Basic Statistics"):      #starting with the First line
            while True:                                 
                try:
                    line=next(file)                     
                    print(line)
                except StopIteration:                 # breaking the iteration once-   
                      break
                if line.startswith(">>END_MODULE"):    #it reaches the last line marking the end of the module,
                        break
                Basic_Statistics.append(line)           #adding it to the 
    
                def mkdir_p(path):                       #make new directory
                     try:
                      os.chdir(select_path)              # change directory to Select_path
                      #print(select_path)
                      
                      os.makedirs(path)
                     except OSError as exec: 
                      if exec.errno == errno.EEXIST and os.path.isdir(path):  # prevent making a new directory if thereis one already existing
                        pass
                def Output_w(path):                     # give an output folder
                    print(path)
                    mkdir_p(os.path.dirname(path))
                    return open(path, 'w')
    print(Module, '\n'.join(Basic_Statistics), file = Output_w(os.path.join(select_path, "output_Directory/Basic Statistics/Basic Statistics.txt"))) # give path for everything to be written in the output directory.
    print("\n".join(Basic_Statistics))
    
""" per base sequence quality from the given file is extracted in a separate list, the loop iterates from the first line (Per base sequence quality)
  to the last line (END_MODULE) and adds the list to the output directory. Further the extracted data is uploaded to numpy for plotting graph."""  
Module=("Per Base Sequence Quality" "\n")
with open(filename, 'r') as file:
    
    Per_Base_Sequence_Quality=[]
    for line in file:
        if line.startswith(">>Per base sequence quality"):
            while True:
                try:
                    line=next(file)
                except StopIteration:
                      break
                if line.startswith(">>END_MODULE"):
                      break
                Per_Base_Sequence_Quality.append(line)
                                
                def mkdir_p(path):
                  try:
                    os.makedirs(path)
                  except OSError as exec: 
                    if exec.errno == errno.EEXIST and os.path.isdir(path):
                      pass
                def Output_w(path):
                    mkdir_p(os.path.dirname(path))
                    return open(path, 'w')
    print(Module, '\n'.join(Per_Base_Sequence_Quality), file = Output_w(os.path.join(select_path, "output_Directory/Per base sequence quality/Per base sequence quality.txt")))
    print("\n".join(Per_Base_Sequence_Quality))
    
    
df = np.loadtxt(Per_Base_Sequence_Quality, dtype='float', usecols = [0,1,2,3,4,5,6], skiprows=1) # loading text from the list to text in numpy saving it as a dataframe.

df1 = pd.DataFrame(df, columns= ['Base', 'Mean', 'Median', 'Lower Quartile', 'Upper Quartile', '10th Percentile',  '90th Percentile'])
#df_1 = np.transpose(df1)
df1 = df1.astype(np.float64)
#df1 = np.transpose(df1)

df_pd = df1.drop(['Base', 'Mean'], axis=1)
df_Me = df1.drop(['Base', 'Median', 'Lower Quartile', 'Upper Quartile', '10th Percentile',  '90th Percentile'], axis=1)

header = df_pd.iloc[0]
df_pd = df_pd[1:]
df_pd.columns=header

# header1 = df_Me.iloc[0]
# df_Me = df_Me[1:]
# df_Me.columns=header1

plt.figure(figsize=(25,10))
plt.boxplot(df_pd,whis= [0, 20], sym='_')

plt.plot(df_Me)    
plt.yticks(np.arange(0, 45, step=4))
plt.xlabel('Position in read(bp)')
plt.ylabel('Quality score')
plt.ylim(0,45)
plt.xlim([0,76])
plt.axhspan(20, 28, facecolor='wheat', alpha=0.7)
plt.axhspan(28, 45, facecolor='lightgreen', alpha=0.6)
plt.axhspan(0, 20, facecolor='salmon', alpha=0.6)
plt.title("Per Base Sequence Quality", fontsize=20)

plt.savefig((os.path.join(select_path,'output_Directory/Per base sequence quality/Graph.jpeg'))) #plot will be saved to the output directory


""" per Sequence Quality Scores from the given file is extracted in a separate list, the loop iterates from the first line (Per sequence quality scores)
 to the last line (END_MODULE) and adds the list ta the output directory assigned. Further the extracted data is uploaded to numpy for plotting graph."""    
Module=("Per sequence quality scores" "\n")
with open(filename, 'r') as file:   # open any fastqc file
    Per_sequence_quality_scores=[]   # create an empty list
    
    for line in file:        
        if line.startswith(">>Per sequence quality scores"):      # start reading the file from the assigned line
            while True:
                try:
                    line=next(file)                           # read the file line by line
                except StopIteration:                         # stop iteration
                      break                                   # and break the loop
                if line.startswith(">>END_MODULE"):           # encountering the end line of the module
                        break
                Per_sequence_quality_scores.append(line)       # append the list to a new directory
                          
                def mkdir_p(path):
                 try:
                  os.makedirs(path)
                 except OSError as exec: 
                  if exec.errno == errno.EEXIST and os.path.isdir(path):
                    pass
                def Output_w(path):
                    mkdir_p(os.path.dirname(path))
                    return open(path, 'w')
print(Module, '\n'.join(Per_sequence_quality_scores), file = Output_w(os.path.join(select_path, "output_Directory/Per sequence quality scores/Per sequence quality scores.txt")))
print("\n".join(Per_sequence_quality_scores))
 
data = np.loadtxt(Per_sequence_quality_scores, dtype='float', usecols = [0,1], skiprows=1) # loading text from the list to text in numpy saving it as a dataframe.
# plotting the graph
x =data[:,0] #assign the value of X
y = data[:,1] #assign the value Y


fig = plt.figure() #plot the figure
major_ticks = np.arange(0, 111,5 ) #numpy returns an arranged by space array
ax1 = fig.add_subplot()

ax1.yaxis.set_major_formatter(FormatStrFormatter('%d'))
ax1.xaxis.set_minor_locator(MultipleLocator())
ax1.set_xticks(major_ticks)

ax1.grid(which='minor', alpha=0.5)
ax1.grid(which='major', alpha=0.5)
ax1.set_title("Per sequence quality scores")    
ax1.set_xlabel('Quality')
ax1.set_ylabel('Count')
ax1.plot(x,y, c='r')
plt.savefig((os.path.join(select_path,'output_Directory/Per sequence quality scores/Graph.jpeg'))) #plot will be saved to the output directory

""" per tile sequence quality from the given file is extracted in a separate list, the loop iterates from the first line (Per tile sequence quality)
 to the last line (END_MODULE) and adds the list ta the output directory. Further the extracted data is uploaded to numpy for plotting graph."""  
Module=("Per tile sequence quality" "\n")
with open(filename, 'r') as file:
    Per_tile_sequence_quality=[]
    for line in file:
        if line.startswith(">>Per tile sequence quality"):
            while True:
                try:
                    line=next(file)
                except StopIteration:
                      break
                if line.startswith(">>END_MODULE"):
                        break
                Per_tile_sequence_quality.append(line)
               
                def mkdir_p(path):
                 try:
                  os.makedirs(path)
                 except OSError as exec: 
                  if exec.errno == errno.EEXIST and os.path.isdir(path):
                    pass
                def Output_w(path):
                    mkdir_p(os.path.dirname(path))
                    return open(path, 'w')
print(Module, '\n'.join(Per_tile_sequence_quality), file = Output_w(os.path.join(select_path, "output_Directory/Per tile sequence quality/Per tile sequence quality.txt")))
print("\n".join(Per_tile_sequence_quality))

""" per base sequence content from the given file is extracted in a separate list, the loop iterates from the first line (Per base sequence content)
 to the last line (END_MODULE) and adds the list ta the output directory. Further the extracted data is uploaded to numpy for plotting graph."""  
Module=("Per base sequence content" "\n")
with open(filename, 'r') as file:
    Per_base_sequence_content=[]
    for line in file:
        if line.startswith(">>Per base sequence content"):
            while True:
                try:
                    line=next(file)
                except StopIteration:
                      break
                if line.startswith(">>END_MODULE"):
                        break
                Per_base_sequence_content.append(line) 
                
                def mkdir_p(path):
                 try:
                  os.makedirs(path)
                 except OSError as exec: 
                  if exec.errno == errno.EEXIST and os.path.isdir(path):
                    pass
                def Output_w(path):
                    mkdir_p(os.path.dirname(path))
                    return open(path, 'w')
print(Module, '\n'.join(Per_base_sequence_content), file = Output_w(os.path.join(select_path, "output_Directory/Per base sequence content/Per base sequence content.txt")))
print("\n".join(Per_base_sequence_content))

#plotting of the graph
data = np.loadtxt(Per_base_sequence_content, dtype='float', usecols = [0,1,2,3,4], skiprows=1)
x =data[:,0]
y = data[:,1]
y1 =data[:,2]
y2 =data[:,3]
y3 =data[:,4]
fig = plt.figure()
major_ticks = np.arange(0, 111, 10)
minor_ticks = np.arange(2, 111, 5)
ax1 = fig.add_subplot()
ax1.set_xticks(major_ticks)
ax1.set_xticks(minor_ticks, minor=True)
ax1.set_yticks(major_ticks)
ax1.set_yticks(minor_ticks, minor=True)
ax1.grid(which='both')
ax1.grid(which='minor', alpha=0.9)
ax1.grid(which='major', alpha=0.5)
ax1.set_title("Per Base Sequence Content")    
ax1.set_xlabel('Base')
plt.plot(x, y, label='G')
plt.plot(x, y1, label='A') 
plt.plot(x, y2, label='T')
plt.plot(x, y3, label='C')
plt.ylim(0,100)
plt.legend()
plt.savefig(os.path.join(select_path, "output_Directory/Per base sequence content/Graph.jpeg"))

""" per sequence GC content from the given file is extracted in a separate list, the loop iterates from the first line (Per sequence GC content)
 to the last line (END_MODULE) and adds the list ta the output directory. Further the extracted data is uploaded to numpy for plotting graph."""  
Module=("Per sequence GC content" "\n")
with open(filename, 'r') as file:
    Per_sequence_GC_content=[]
    for line in file:
        if line.startswith(">>Per sequence GC content"):
            while True:
                try:
                    line=next(file)
                except StopIteration:
                      break
                if line.startswith(">>END_MODULE"):
                        break
                Per_sequence_GC_content.append(line)
                                
                def mkdir_p(path):
                 try:
                  os.makedirs(path)
                 except OSError as exec: 
                  if exec.errno == errno.EEXIST and os.path.isdir(path):
                    pass
                def Output_w(path):
                    mkdir_p(os.path.dirname(path))
                    return open(path, 'w')
                
print(Module, '\n'.join(Per_sequence_GC_content), file = Output_w(os.path.join(select_path, "output_Directory/Per sequence GC content/Per sequence GC content.txt")))
print("\n".join(Per_sequence_GC_content))
# plot graph
data = np.loadtxt(Per_sequence_GC_content, dtype='float', usecols = [0,1], skiprows=1)
x =data[:,0]
y = data[:,1]
fig = plt.figure()
major_ticks = np.arange(0, 111,10 )
ax1 = fig.add_subplot()
ax1.yaxis.set_major_formatter(FormatStrFormatter('%d'))
# For the minor ticks,I use no labels; Because it has a default NullFormatter.
ax1.xaxis.set_minor_locator(MultipleLocator())
ax1.set_xticks(major_ticks)
ax1.grid(which='both')
#its for changing the grid settings:
ax1.grid(which='minor', alpha=0.5)
ax1.grid(which='major', alpha=0.5)
ax1.set_title("Per sequence GC content")    
ax1.set_xlabel('GC Present')
ax1.set_ylabel('Total Content Count')
ax1.plot(x,y, c='r')
plt.savefig(os.path.join(select_path, "output_Directory/Per sequence GC content/Graph.jpeg"))

""" per base N content from the given file is extracted in a separate list, the loop iterates from the first line (Per base N content)
 to the last line (END_MODULE) and adds the list ta the output directory. Further the extracted data is uploaded to numpy for plotting graph."""    
Module=("Per base N content" "\n")
with open(filename, 'r') as file:
    Per_base_N_content=[]
    for line in file:
        if line.startswith(">>Per base N content"):
            while True:
                try:
                    line=next(file)
                except StopIteration:
                      break
                if line.startswith(">>END_MODULE"):
                        break
                Per_base_N_content.append(line)
                                
                def mkdir_p(path):
                 try:
                  os.makedirs(path)
                 except OSError as exec: 
                  if exec.errno == errno.EEXIST and os.path.isdir(path):
                    pass
                def Output_w(path):
                    mkdir_p(os.path.dirname(path))
                    return open(path, 'w')
                
print(Module, '\n'.join(Per_base_N_content), file = Output_w(os.path.join(select_path, "output_Directory/Per base N content/Content.txt")))
print("\n".join(Per_base_N_content))
    
#plotting the graph
data = np.loadtxt(Per_base_N_content, dtype='float', usecols = [0,1], skiprows=1)
x =data[:,0]
y = data[:,1]
fig = plt.figure()
major_ticks = np.arange(0, 111, 5)
ax1 = fig.add_subplot()
ax1.set_xticks(major_ticks)

# Or if you want different settings for the grids:
ax1.grid(which='minor', alpha=0.5) # returns minor number
ax1.grid(which='major', alpha=0.5) # returns major number

ax1.set_title("Per Base N Content")    
ax1.set_xlabel('Base')
ax1.set_ylabel('N-Counts')
      
ax1.plot(x,y, c='r') # changing colour red
plt.savefig(os.path.join(select_path, "output_Directory/Per base N content/Graph.jpeg"))

""" Sequence length distribution from the given file is extracted in a separate list, the loop iterates from the first line (Sequence Length Distribution)
 to the last line (END_MODULE) and adds the list ta the output directory. Further the extracted data is uploaded to numpy for plotting graph."""    
Module=("Sequence Length Distribution" "\n")
with open(filename, 'r') as file:
    Sequence_Length_Distribution=[]
    for line in file:
        if line.startswith(">>Sequence Length Distribution"):
            while True:
                try:
                    line=next(file)
                except StopIteration:
                      break
                if line.startswith(">>END_MODULE"):
                        break
                Sequence_Length_Distribution.append(line)
                                
                def mkdir_p(path):
                 try:
                  os.makedirs(path)
                 except OSError as exec: 
                  if exec.errno == errno.EEXIST and os.path.isdir(path):
                    pass
                def Output_w(path):
                    mkdir_p(os.path.dirname(path))
                    return open(path, 'w')
                
print(Module, '\n'.join(Sequence_Length_Distribution), file = Output_w(os.path.join(select_path, "output_Directory/Sequence Length Distribution/Sequence Length Distribution.txt")))
print("\n".join(Sequence_Length_Distribution))
print(Sequence_Length_Distribution)
# plot graph
# data = pd.read_csv(os.path.join(select_path, "output_Directory/Sequence Length Distribution/Sequence Length Distribution.txt"), delim_whitespace=True, header=None, skiprows=1, comment='#', engine='python')
# data.columns = ["Length", "Count"]
# print(data)

# fig=plt.figure()

# plt.plot(data['Length'],data['Count'], marker='x',linestyle='dashed', linewidth=2, markersize=12)
# fig.suptitle("Sequence Length Distribution")
# #plt.set_title("Sequence Length Distribution")
# plt.savefig(os.path.join(select_path, "output_Directory/Sequence Length Distribution/Graph.jpeg"))

""" Sequence duplication levels from the given file is extracted in a separate list, the loop iterates from the first line (sequence duplication levels)
 to the last line (END_MODULE) and adds the list ta the output directory. Further the extracted data is uploaded to numpy for plotting graph."""  
Module=("Sequence Duplication Levels" "\n")
with open(filename, 'r') as file:
    Sequence_Duplication_Levels=[]
    for line in file:
        if line.startswith(">>Sequence Duplication Levels"):
            while True:
                try:
                    line=next(file)
                except StopIteration:
                      break
                if line.startswith(">>END_MODULE"):
                        break
                Sequence_Duplication_Levels.append(line)
                                
                def mkdir_p(path):
                 try:
                  os.makedirs(path)
                 except OSError as exec: # Python >2.5
                  if exec.errno == errno.EEXIST and os.path.isdir(path):
                    pass
                def Output_w(path):
                    mkdir_p(os.path.dirname(path))
                    return open(path, 'w')
print(Module, '\n'.join(Sequence_Duplication_Levels), file =Output_w(os.path.join(select_path, "output_Directory/Sequence Duplication Levels/Sequence Duplication Levels.txt")))
print(Module)
print("\n".join(Sequence_Duplication_Levels))

# plot graph  
data = pd.read_csv(os.path.join(select_path, "output_Directory/Sequence Duplication Levels/Sequence Duplication Levels.txt"), delim_whitespace=True, header=None, skiprows=1, comment='#', engine='python')
data.columns = ["Duplication Level", "Percentage deduplicated", "Percentage of total"]
print(data)
fig = plt.figure()
plt.plot(data['Duplication Level'],data['Percentage deduplicated'],data['Percentage of total'])
fig.suptitle("Sequence Duplication Levels")
#plt.set_title("Sequence Duplication Levels")
plt.savefig(os.path.join(select_path, "output_Directory/Sequence Duplication Levels/Graph.jpeg"))

""" Overrepresented sequences from the given file is extracted in a separate list, the loop iterates from the first line (Overrepresented sequences)
 to the last line (END_MODULE) and adds the list ta the output directory. Further the extracted data is uploaded to numpy for plotting graph."""  
Module=("Overrepresented sequences" "\n")
with open(filename, 'r') as file:
    Overrepresented_sequences=[]
    for line in file:
        if line.startswith(">>Overrepresented sequences"):
            while True:
                try:
                    line=next(file)
                except StopIteration:
                      break
                if line.startswith(">>END_MODULE"):
                        break
                Overrepresented_sequences.append(line)
                       
                def mkdir_p(path):
                 try:
                  os.makedirs(path)
                 except OSError as exec: 
                  if exec.errno == errno.EEXIST and os.path.isdir(path):
                    pass
                def Output_w(path):
                    mkdir_p(os.path.dirname(path))
                    return open(path, 'w')
                
print(Module, '\n'.join(Overrepresented_sequences), file = Output_w(os.path.join(select_path, "output_Directory/Overrepresented sequences/Overrepresented sequences.txt")))
print("\n".join(Overrepresented_sequences))

""" Adapter content from the given file is extracted in a separate list, the loop iterates from the first line (Adapter content)
 to the last line (END_MODULE) and adds the list ta the output directory. Further the extracted data is uploaded to numpy for plotting graph."""  
Module=("Adapter Content" "\n")
with open(filename, 'r') as file:
    Adapter_Content=[]
    for line in file:
        if line.startswith(">>Adapter Content"):
            while True:
                try:
                    line=next(file)
                except StopIteration:
                      break
                if line.startswith(">>END_MODULE"):
                        break
                Adapter_Content.append(line)
                                
                def mkdir_p(path):
                 try:
                  os.makedirs(path)
                 except OSError as exec: 
                  if exec.errno == errno.EEXIST and os.path.isdir(path):
                    pass
                def Output_w(path):
                    mkdir_p(os.path.dirname(path))
                    return open(path, 'w')
print(Module, '\n'.join(Adapter_Content), file = Output_w(os.path.join(select_path, "output_Directory/Adapter Content/Adapter_Content.txt")))
print("\n".join(Adapter_Content))
   
data = np.loadtxt(Adapter_Content, dtype='float', usecols = [0,1,2,3,4], skiprows=1) #load to numpy with 5 columns and skip the first row
#declare the X and all the Y axis
x =data[:,0]
y = data[:,1]
ya =data[:,2]
yb =data[:,3]
yc =data[:,4]
# create a new figure   
fig = plt.figure()
major_ticks = np.arange(0, 111, 5)
ax1 = fig.add_subplot()
ax1.set_xticks(major_ticks)
ax1.set_yticks(major_ticks)
ax1.grid(which='both')
# Or if you want different settings for the grids:
ax1.grid(which='minor', alpha=0.5)
ax1.grid(which='major', alpha=0.5)
ax1.set_title("Adapter_Content")    
ax1.set_xlabel('Position in read')
plt.plot(x, y, label='Illumina Universal Adapter')
plt.plot(x, ya, label='Illumina Small RNA Adapter') 
plt.plot(x, yb, label='Nextera Transposase Sequence')
plt.plot(x, yc, label='SOLID Small RNA Adapter') 
plt.xticks(np.arange(min(x), max(x), 5))
plt.yticks(np.arange(min(y), max(y), 0.005))
plt.legend()
plt.savefig(os.path.join(select_path, "output_Directory/Adapter Content/Graph.jpeg"))

""" Kmer content from the given file is extracted in a separate list, the loop iterates from the first line (Kmer content)
 to the last line (END_MODULE) and adds the list to the output directory. Further the extracted data is uploaded to numpy for plotting graph.""" 
Module=("Kmer Content" "\n")
with open(filename, 'r') as file:
   Kmer_Content=[]
   for line in file:
       if line.startswith(">>Kmer Content"):
           while True:
               try:
                   line=next(file)
               except StopIteration:
                     break
               if line.startswith(">>END_MODULE"):
                       break
               Kmer_Content.append(line)
                               
               def mkdir_p(path):
                try:
                 os.makedirs(path)
                except OSError as exec: 
                 if exec.errno == errno.EEXIST and os.path.isdir(path):
                   pass
               def Output_w(path):
                   mkdir_p(os.path.dirname(path))
                   return open(path, 'w')
print(Module, '\n'.join(Kmer_Content), file = Output_w(os.path.join(select_path, "output_Directory/Kmer Content/Kmer_Content.txt")))
print(Module)
print("\n".join(Kmer_Content))

#the plotting of the Kc graph, the problem the graph has is that we can plot it directly from the list[], 
# therefore this mean we have to hardcode the location of the extracted Kc
data = pd.read_csv(os.path.join(select_path, "output_Directory/Kmer Content/Kmer_Content.txt"),delim_whitespace=True, header=None, skiprows=1, comment='#', engine='python')
data.columns = ["Sequence", "Count", "PValue", "Obs/Exp Max", "Max Obs/Exp Position"]
print(data)

fig = plt.figure()
plt.plot(data['Sequence'],data['Count'],data['PValue'],data['Obs/Exp Max'],data['Max Obs/Exp Position'], marker='o')
fig.suptitle("Kmer Graph")
#plt.set_title("Kmer Graph")
plt.savefig(os.path.join(select_path, "output_Directory/Kmer Content/Graph.jpeg"))
  



  

