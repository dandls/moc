
openmlids := $(shell ls ../../saved_objects/benchmark/)
datafiles := $(shell find ../../saved_objects/benchmark/*/data_orig.csv)
datafiles_logreg := $(shell find ../../saved_objects/benchmark/*/data_orig.csv | grep -E "boston|diabetes|ilpd|kc2|no2|pc1")
ncf = 10 
all : recourse-benchmark dice-benchmark

venv-dice :
	python3 -m venv venv-dice

DiCE :
	git clone https://github.com/microsoft/DiCE.git;
	cd DiCE; git reset --hard 7bcb098;

install-dice : venv-dice
	venv-dice/bin/pip3 install --upgrade pip
	cd DiCE; ../venv-dice/bin/python3 setup.py install
	venv-dice/bin/pip3 install click 
	venv-dice/bin/pip3 install --upgrade tensorflow


dicefiles-logreg := $(patsubst %data_orig.csv, %cf-dice-logreg.csv,$(datafiles_logreg))
dicefiles-neuralnet := $(patsubst %data_orig.csv, %cf-dice-neuralnet.csv,$(datafiles))

dice-benchmark: $(dicefiles-logreg) $(dicefiles-neuralnet) cf-dice.py

$(dicefiles-logreg) : ../../saved_objects/benchmark/%/cf-dice-logreg.csv: ../../saved_objects/benchmark/%/data_orig.csv
	-venv-dice/bin/python3 cf-dice.py --openmlid $* --model logreg --ncf $(ncf)

$(dicefiles-neuralnet) : ../../saved_objects/benchmark/%/cf-dice-neuralnet.csv: ../../saved_objects/benchmark/%/data_orig.csv
	-venv-dice/bin/python3 cf-dice.py --openmlid $* --model neuralnet --ncf $(ncf)


venv-recourse : 
	python3 -m venv venv-recourse

install-recourse : venv-recourse
	#git clone git@github.com:ustunb/actionable-recourse.git
	venv-recourse/bin/pip3 install actionable-recourse
	# Make sure to download ibm CPLEX and install it into /opt/ibm/ILOG
	# There is a free academic / student version of CPLEX
	venv-recourse/bin/python3 /opt/ibm/ILOG/CPLEX_Studio1210/python/setup.py install
	#cd actionable-recourse; git reset --hard aaae8fa; ../venv-recourse/bin/python3 setup.py install
	venv-dice/bin/pip install --upgrade pip
	venv-recourse/bin/pip3 install click 
	venv-dice/bin/pip3 install --upgrade tensorflow

recoursefiles := $(patsubst %data_orig.csv, %cf-recourse-logreg.csv,$(datafiles_logreg))

recourse-benchmark : $(recoursefiles) cf-recourse.py

$(recoursefiles) : ../../saved_objects/benchmark/%/cf-recourse-logreg.csv: ../../saved_objects/benchmark/%/data_orig.csv
	venv-recourse/bin/python3 cf-recourse.py --openmlid $* --ncf $(ncf) 


