SRC_DIR = src
HI_DIR = hidir
O_DIR = odir
MAIN_SRC = $(SRC_DIR)/VerilogFiddle.hs
SRCS = $(MAIN_SRC)
SRCS += $(SRC_DIR)/VerilogFiddle/InterfaceInference.hs
SRCS += $(SRC_DIR)/VerilogFiddle/Parse.hs
SRCS += $(SRC_DIR)/VerilogFiddle/Types.hs

.PHONY: all

all: verilog-fiddle

verilog-fiddle: $(SRCS)
	ghc --make -j \
        -hidir $(HI_DIR) -odir $(O_DIR) -i$(SRC_DIR) \
		-o $@ \
        $(MAIN_SRC)

.PHONY: clean mrproper

clean:
	rm -rf $(HI_DIR) $(O_DIR)

mrproper: clean
	rm -f verilog-fiddle
