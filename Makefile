.PHONY: install files dirs backup

ignores := Makefile README TODO Rakefile %~

dirs := $(filter-out $(ignores), $(subst /,,$(wildcard */)))
files := $(filter-out $(ignores) $(subst /,,$(dirs)), $(wildcard *))

dir_targets := $(addprefix install_dir_, $(dirs))
file_targets := $(addprefix install_file_, $(files))

install: files dirs

files: $(file_targets)

dirs: $(dir_targets)


define DIR_tmpl
install_dir_$(1):
	install -dv $(HOME)/.$(1)
	rsync -avu --exclude '*~' --exclude '*.elc' $(1) $(HOME)/.$(1)
endef

define FILE_tmpl
install_file_$(1):
	@install -Cv $(1) $(HOME)/.$(1)
endef

$(foreach dir, $(dirs), $(eval $(call DIR_tmpl,$(dir))))
$(foreach file, $(files), $(eval $(call FILE_tmpl,$(file))))

#find_files = $(filter-out $(ignore_patterns), $(wildcard $(dir)/*))
#subfiles = $(foreach dir,$(1), $(find_files) $(call subfiles, $(find_files)))
#sub:
#	@echo $(call subfiles, emacs.d)
