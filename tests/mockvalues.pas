unit MockValues;

{$mode ObjFPC}{$H+}

interface

const
    API_USAGE_RESPONSE: string = '{' +
        '"total": 7,' +
        '"status_valid": 5,' +
        '"status_invalid": 0,' +
        '"status_catch_all": 0,' +
        '"status_do_not_mail": 3,' +
        '"status_spamtrap": 0,' +
        '"status_unknown": 0,' +
        '"sub_status_toxic": 0,' +
        '"sub_status_disposable": 0,' +
        '"sub_status_role_based": 0,' +
        '"sub_status_possible_trap": 0,' +
        '"sub_status_global_suppression": 3,' +
        '"sub_status_timeout_exceeded": 0,' +
        '"sub_status_mail_server_temporary_error": 0,' +
        '"sub_status_mail_server_did_not_respond": 0,' +
        '"sub_status_greylisted": 0,' +
        '"sub_status_antispam_system": 0,' +
        '"sub_status_does_not_accept_mail": 0,' +
        '"sub_status_exception_occurred": 0,' +
        '"sub_status_failed_syntax_check": 0,' +
        '"sub_status_mailbox_not_found": 0,' +
        '"sub_status_unroutable_ip_address": 0,' +
        '"sub_status_possible_typo": 0,' +
        '"sub_status_no_dns_entries": 0,' +
        '"sub_status_role_based_catch_all": 0,' +
        '"sub_status_mailbox_quota_exceeded": 0,' +
        '"sub_status_forcible_disconnect": 0,' +
        '"sub_status_failed_smtp_connection": 0,' +
        '"sub_status_mx_forward": 0,' +
        '"sub_status_alternate": 1,' +
        '"sub_status_blocked": 0,' +
        '"sub_status_allowed": 0,' +
        '"start_date": "4/1/2023",' +
        '"end_date": "4/20/2023"' +
    '}';
    ERROR_MESSAGE: string = 'Mock error message';
    ERROR_PAYLOAD: string = '{"error": "Mock error message"}';
    ACTIVITY_DATA_FOUND: string = '{"found": true, "active_in_days": "180"};';
    ACTIVITY_DATA_NOT_FOUND: string = '{"found": false, "active_in_days": null};';

    VALDATION_RESPONSE_VALID: string = '{' +
        '    "address": "valid@example.com",' +
        '    "status": "valid",' +
        '    "sub_status": "",' +
        '    "free_email": false,' +
        '    "did_you_mean": null,' +
        '    "account": null,' +
        '    "domain": null,' +
        '    "domain_age_days": "9692",' +
        '    "smtp_provider": "example",' +
        '    "mx_found": "true",' +
        '    "mx_record": "mx.example.com",' +
        '    "firstname": "zero",' +
        '    "lastname": "bounce",' +
        '    "gender": "male",' +
        '    "country": null,' +
        '    "region": null,' +
        '    "city": null,' +
        '    "zipcode": null,' +
        '    "processed_at": "2023-04-25 13:08:24.269"' +
        '}';

    VALDATION_RESPONSE_INVALID: string ='{' +
        '    "address": "invalid@example.com",' +
        '    "status": "invalid",' +
        '    "sub_status": "mailbox_not_found",' +
        '    "free_email": false,' +
        '    "did_you_mean": null,' +
        '    "account": null,' +
        '    "domain": null,' +
        '    "domain_age_days": "9692",' +
        '    "smtp_provider": "example",' +
        '    "mx_found": "true",' +
        '    "mx_record": "mx.example.com",' +
        '    "firstname": "zero",' +
        '    "lastname": "bounce",' +
        '    "gender": "male",' +
        '    "country": null,' +
        '    "region": null,' +
        '    "city": null,' +
        '    "zipcode": null,' +
        '    "processed_at": "2023-12-25 13:08:24.001"' +
        '}';

    BATCH_VALIDATE_OK: string = '{' +
        '    "email_batch": [' +
        '{' +
        '    "address": "valid@example.com",' +
        '    "status": "valid",' +
        '    "sub_status": "",' +
        '    "free_email": false,' +
        '    "did_you_mean": null,' +
        '    "account": null,' +
        '    "domain": null,' +
        '    "domain_age_days": "9692",' +
        '    "smtp_provider": "example",' +
        '    "mx_found": "true",' +
        '    "mx_record": "mx.example.com",' +
        '    "firstname": "zero",' +
        '    "lastname": "bounce",' +
        '    "gender": "male",' +
        '    "country": null,' +
        '    "region": null,' +
        '    "city": null,' +
        '    "zipcode": null,' +
        '    "processed_at": "2023-04-25 13:08:24.269"' +
        '}, ' +
        '{' +
        '    "address": "invalid@example.com",' +
        '    "status": "invalid",' +
        '    "sub_status": "mailbox_not_found",' +
        '    "free_email": false,' +
        '    "did_you_mean": null,' +
        '    "account": null,' +
        '    "domain": null,' +
        '    "domain_age_days": "9692",' +
        '    "smtp_provider": "example",' +
        '    "mx_found": "true",' +
        '    "mx_record": "mx.example.com",' +
        '    "firstname": "zero",' +
        '    "lastname": "bounce",' +
        '    "gender": "male",' +
        '    "country": null,' +
        '    "region": null,' +
        '    "city": null,' +
        '    "zipcode": null,' +
        '    "processed_at": "2023-12-25 13:08:24.001"' +
        '}' +
        '    ],' +
        '    "errors": []' +
        '}';

    BATCH_VALIDATE_ERROR_SAMPLE: string = '{' +
        '    "email_address": "all",' +
        '    "error": "Invalid API Key or your account ran out of credits"' +
        '}';

    BATCH_VALIDATE_ERROR: string = '{' +
        '    "email_batch": [],' +
        '    "errors": [' +
        '        {' +
        '            "email_address": "all",' +
        '            "error": "Invalid API Key or your account ran out of credits"' +
        '        }' +
        '    ]' +
        '}';


    BULK_SUBMIT_OK = '{' +
        '    "success": true,' +
        '    "message": "File Accepted",' +
        '    "file_name": "emails2.txt",' +
        '    "file_id": "e90e9b1d-8dc7-40eb-a7d9-999d52086a56"' +
        '}';


    BULK_STATUS_OK = '{' +
        '    "success": true,' +
        '    "file_id": "e90e9b1d-8dc7-40eb-a7d9-999d52086a56",' +
        '    "file_name": "emails2.txt",' +
        '    "upload_date": "2023-04-26T17:52:23Z",' +
        '    "file_status": "Processing",' +
        '    "complete_percentage": "100%",' +
        '    "error_reason": null,' +
        '    "return_url": "https://mock.value.com/"' +
        '}';

    BULK_RESULT_DELETED = '{ "success": false, "message": "File deleted." }';
    BULK_DELETE_OK = '{' +
        '    "success": true,' +
        '    "message": "File Deleted",' +
        '    "file_name": "emails2.txt",' +
        '    "file_id": "e90e9b1d-8dc7-40eb-a7d9-999d52086a56"' +
        '}';

    BULK_DELETE_NOT_FOUND = '{ "success": false, "message": "File cannot be found."}';

    MOCK_FIND_MAIL_STATUS_INVALID: string = '{' +
        '    "email": "",' +
        '    "domain": "example.com",' +
        '    "format": "unknown",' +
        '    "status": "invalid",' +
        '    "sub_status": "no_dns_entries",' +
        '    "confidence": "undetermined",' +
        '    "did_you_mean": "",' +
        '    "failure_reason": "",' +
        '    "other_domain_formats": []' +
        '}';

    MOCK_FIND_MAIL_STATUS_VALID: string = '{' +
        '    "email": "john.doe@example.com",' +
        '    "domain": "example.com",' +
        '    "format": "first.last",' +
        '    "status": "valid",' +
        '    "sub_status": "",' +
        '    "confidence": "high",' +
        '    "did_you_mean": "",' +
        '    "failure_reason": "",' +
        '    "other_domain_formats": [' +
        '        {' +
        '            "format": "first_last",' +
        '            "confidence": "high"' +
        '        },' +
        '        {' +
        '            "format": "first",' +
        '            "confidence": "medium"' +
        '        }' +
        '    ]' +
        '}';


implementation

end.

