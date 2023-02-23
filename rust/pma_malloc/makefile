#==============================================================================
# MACROS
#==============================================================================

CC := gcc
CSTD := -std=c11
LIB_CFLAGS := -D_GNU_SOURCE
DEV_CFLAGS := -Wall -Wextra -Wpedantic -Wformat=2 -Wno-unused-parameter \
             -Wshadow -Wwrite-strings -Wstrict-prototypes \
             -Wold-style-definition -Wredundant-decls -Wnested-externs \
             -Wmissing-include-dirs -Og

# Core sources
SRC_DIR := src/
OBJ_DIR := obj/
DEP_DIR := dep/
BIN_DIR := bin/

SOURCES := $(shell ls $(SRC_DIR)*.c)
OBJECTS := $(subst $(SRC_DIR),$(OBJ_DIR),$(subst .c,.o,$(SOURCES)))
DEPFILES := $(subst $(SRC_DIR),$(DEP_DIR),$(subst .c,.d,$(SOURCES)))

# Additional sources
INC_DIR := includes/
INC_SRC_DIR := ${addprefix ${SRC_DIR},${INC_DIR}}
INC_OBJ_DIR := ${addprefix ${OBJ_DIR},${INC_DIR}}
INC_DEP_DIR := ${addprefix ${DEP_DIR},${INC_DIR}}

INC_SOURCES := $(shell ls $(INC_SRC_DIR)*.c)
INC_OBJECTS := $(subst $(INC_SRC_DIR),$(INC_OBJ_DIR),$(subst .c,.o,$(INC_SOURCES)))
INC_DEPFILES := $(subst $(INC_SRC_DIR),$(INC_DEP_DIR),$(subst .c,.d,$(INC_SOURCES)))

# Tests
CMD_NAME := sanity

TEST_DIR := ${BIN_DIR}test/
TST_SRC_DIR := $(addprefix $(SRC_DIR),"test/")

TEST_SRC := $(addprefix $(TST_SRC_DIR),$(addsuffix .c,$(CMD_NAME)))
TEST_CMD := $(addprefix $(BIN_DIR),$(CMD_NAME))

#==============================================================================
# TARGETS
#==============================================================================

#
# DEFAULT TARGET
#

default : help

#
# VISIBLE TARGETS
#

# Helpful rule which lists all other rules and encourages documentation
#
# target: help - Display all targets in makefile
#
help :
	@egrep "^# target:" makefile

# Run sanity check
#
# target: sane - Run sanity check
sane : $(TEST_CMD)
	@$(TEST_CMD) $(TEST_DIR)

# Clean up files produced by the makefile. Any invocation should execute, regardless of file modification date, hence
# dependency on FRC.
#
# target: clean - Remove all files produced by this makefile
clean : FRC
	@rm -rf $(BIN_DIR) $(OBJ_DIR) $(DEP_DIR)

#
# HIDDEN TARGETS
#

# Force build of dependency and object files to import additional makefile targets
#
-include $(DEPFILES) ${INC_DEPFILES}

# Link executable binary for sanity check
#
$(TEST_CMD) : $(OBJECTS) ${INC_OBJECTS}
	@mkdir -p $(BIN_DIR)
	$(CC) $(TEST_SRC) $^ -o $@ $(CSTD) $(LIB_CFLAGS) $(DEV_CFLAGS)

# Compile all source files, but do not link. As a side effect, compile a dependency file for each source file.
#
# Dependency files are a common makefile feature used to speed up builds by auto-generating granular makefile targets.
# These files minimize the number of targets that need to be recomputed when source files are modified and can lead to
# massive build-time improvements.
#
# For more information, see the "-M" option documentation in the GCC man page, as well as this paper:
# https://web.archive.org/web/20150319074420/http://aegis.sourceforge.net/auug97.pdf
#
$(addprefix $(DEP_DIR),%.d) : $(addprefix $(SRC_DIR),%.c)
	@mkdir -p $(OBJ_DIR)
	@mkdir -p $(DEP_DIR)
	$(CC) -MD -MP -MF $@ -MT '$@ $(subst $(DEP_DIR),$(OBJ_DIR),$(@:.d=.o))' \
		$< -c -o $(subst $(DEP_DIR),$(OBJ_DIR),$(@:.d=.o)) $(CSTD) $(LIB_CFLAGS) $(DEV_CFLAGS)

# Same as above, but for additional dependencies.
$(addprefix $(INC_DEP_DIR),%.d) : $(addprefix $(INC_SRC_DIR),%.c)
	@mkdir -p $(INC_OBJ_DIR)
	@mkdir -p $(INC_DEP_DIR)
	$(CC) -MD -MP -MF $@ -MT '$@ $(subst $(INC_DEP_DIR),$(INC_OBJ_DIR),$(@:.d=.o))' \
		$< -c -o $(subst $(INC_DEP_DIR),$(INC_OBJ_DIR),$(@:.d=.o)) $(CSTD) $(LIB_CFLAGS) $(DEV_CFLAGS)

# Special pseudo target which always needs to be recomputed. Forces full rebuild of target every time when used as a
# component.
FRC :
