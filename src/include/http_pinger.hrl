-record(pinger, {
	url :: string(),
	headers :: httpc:headers(),
	post :: [{ string() , binary() }],
	cookie :: [{string(), string()}],
	timeout :: integer() | infinity,
	connect_timeout :: integer() | infinity,
	period :: integer() | infinity
}).

-record(stats, {
	status_counter :: dict(),
	response_time_min = 0 :: integer(),
	response_time_max = 0 :: integer(),
	response_time_sum = 0 :: integer(),
	timeout = 0 :: integer(),
	connect_timeout = 0 :: integer(),
	count_request = 0 :: integer()
}).