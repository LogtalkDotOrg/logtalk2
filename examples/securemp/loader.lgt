
:- initialization((
	catch(logtalk_load(rule_a, [report(on)]), _, true),
	catch(logtalk_load(rule_a_variant, [report(on)]), _, true),
	logtalk_load(rule_b_1),
	logtalk_load(rule_b_2),
	catch(logtalk_load(rule_b_3, [report(on)]), _, true),
	catch(logtalk_load(rule_b_3_variant, [report(on)]), _, true),
	catch(logtalk_load(rule_c, [report(on)]), _, true),
	catch(logtalk_load(rule_d, [report(on)]), _, true),
	catch(logtalk_load(rule_e, [report(on)]), _, true)
)).
