library ZeroBounce;

{$mode objfpc}{$H+}

uses
        Classes,
	    ZbGeneric,
	    ZbBulk,
	    ZbValidation,
	    ZbStructures,
	    ZbUtility;

exports
    // API methods
    ZbGetCredits,
    ZbGetApiUsage,
    ZbGetApiUsageWithin,
    ZbValidateEmail,
    ZbValidateEmailAndIp,
    ZbBatchValidateEmails,
    ZbBatchValidateEmailsAndIp,
    ZbBulkValidationFileSubmit,
    ZbBulkValidationFileStatusCheck,
    ZbBulkValidationResultFetch,
    ZbBulkValidationResultDelete,
    ZbAiScoringFileSubmit,
    ZbAiScoringFileStatusCheck,
    ZbAiScoringResultFetch,
    ZbAiScoringResultDelete,

    // auxiliary
    ZBSetApiKey,
    TApiUsage,
    TZbValidationResult,
    TZbBatchError,
    TZbBatchValidation,
    TZbFileFeedback,
    TZbFileStatus,
    TZbBulkResponse;


end.

