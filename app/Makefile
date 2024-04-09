# Get all Haskell files in the current directory
HASKELL_FILES := $(wildcard *.hs)

# Define the compiler
HC = ghc

# Define flags for the compiler
HC_FLAGS = -w

# Define the target executable name
TARGET = main

# Define the default target
all: clean $(TARGET) remove run

# Rule to build the executable
$(TARGET): $(HASKELL_FILES)
	$(HC) $(HC_FLAGS) -o $(TARGET).out $(HASKELL_FILES)

# Rule to clean up generated files
clean:
	rm -f $(TARGET) *.o *.hi *.out

remove:
	rm -f *.o *.hi

run:
	./main.out