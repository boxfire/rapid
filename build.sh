make rapid && ./rapid && llc second.output.ll&& opt -inline -mem2reg second.output.ll -S |tee second.opt.ll && llc second.opt.ll&& bat second.opt.s
