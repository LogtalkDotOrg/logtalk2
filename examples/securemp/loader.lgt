
:- initialization((
	catch(logtalk_load(rule_a, [report(on)]), _, true),
	catch(logtalk_load(rule_a_variant, [report(on)]), _, true),
	logtalk_load(rule_b_1),
	logtalk_load(rule_b_2),
	catch(logtalk_load(rule_b_3, [report(on)]), _, true),
	catch(logtalk_load(rule_b_3_variant, [report(on)]), _, true),
	catch(logtalk_load(rule_c, [report(on)]), _, true),
	catch(logtalk_load(rule_d, [report(on)]), _, true)
)).



/*
	catch(logtalk_load(lgtmthdredef, [report(on)]), _, true),
	catch(logtalk_load(invclause, [report(on)]), _, true),
	catch(logtalk_load(unknowndir, [report(on)]), _, true),
	catch(logtalk_load(noninstdir, [report(on)]), _, true),
	catch(logtalk_load(invargdir, [report(on)]), _, true),
	catch(logtalk_load(unmatchdir, [report(on)]), _, true),
	catch(logtalk_load(catdynpred, [report(on)]), _, true),
	catch(logtalk_load(ccredef, [report(on)]), _, true),
	catch(logtalk_load(usesrepeated, [report(on)]), _, true),
	catch(logtalk_load(usesconflict, [report(on)]), _, true))).
*/
