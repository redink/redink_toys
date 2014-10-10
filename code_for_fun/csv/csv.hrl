-record(csv, {sep = ",", quote = "\"", eor = "\n",
	      cb_make = undefined, cb_parse = undefined}).

-type csv() :: #csv{}.
