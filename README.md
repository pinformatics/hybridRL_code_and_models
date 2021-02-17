# hybridRL_code_and_models

### Record Linkage
Record linkage refers to identifying the same entities across one or more databases when there is no unique identifier.

### Hybrid Record Linkage
Our Hybrid Record Linkage framework combines the automatic and manual review process to achieve both scalability and high quality linkage results by allowing the automated algorithms to resolve majority of the linkages that have a high probability of being either a match or non-match, but also have the option to send ambiguous pairs to human experts for final determination to improve the linkage quality.
You can use our trained models to conduct record linkage on your data or train a new model using your own dataset.

### How to run the code
 1. Prepareyour pair files (same format as NC_sample_raw_pairs.csv file in sample datafolder)
 2. Run code\1_feature_extraction.R to extract features from your pairs. 
 3. If you want to use n2v feature for first name and last name, run code\add_feature_name2vec.py on the output from step 2 and then run code\2_n2v_postprocess.R 
 4. Run code\3_*_model_*.R to train different ML RL models with your own data.
 6. Run code\5_*_test_NC*.R to test models on testing data.

