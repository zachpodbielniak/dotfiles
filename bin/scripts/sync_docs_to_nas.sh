#!/bin/bash
rsync \
	-ruhv \
	--progress \
	/var/home/zach/Documents/Sync/* \
	/var/mnt/NAS/Home/Users/zach/Documents/
