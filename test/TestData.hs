{-# LANGUAGE QuasiQuotes,
             OverloadedStrings
             #-}

-----------------------------------------------------------------------------
--
-- Module    :  TestData
-- Copyright   :
-- License   :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module TestData  where

import Text.RawString.QQ
import Data.Text

lookUpFileContent :: Text
lookUpFileContent = "performance_test_db\n\
             \\tfor AU: LEMABC923SQL11\n\
            \\tfor NZ: LEMABC923SQL03\n\
            \\n\
            \resynch_stuff_please_AB\n\
            \\t??\n"

incompleteLinkDefinition :: Text
incompleteLinkDefinition = doubleSpaceToTab [r|copy_file _fileName _server
  ??
|]

defectLinkDefinition :: Text
defectLinkDefinition = doubleSpaceToTab [r|copy_file _fileName _server
    # copy _fileName from ..\supporting files\_fileName to _server\dropFolder
    => the file should dissapear within around 30 seconds as it is picked up by the service
|]

doubleTabbedSteps :: Text
doubleTabbedSteps = doubleSpaceToTab [r|steps:
   # first step
     # indented step
     simple_steps
   # final step
|]

simpleNestedTestSteps :: Text
simpleNestedTestSteps = doubleSpaceToTab [r|simple_steps
  # this is linked command one
  => this is linked command two
|]

packReplace :: (Text -> Text) -> (String -> Text)
packReplace replacer = replacer .  pack

doubleSpaceToTab :: String -> Text
doubleSpaceToTab = packReplace addTabs

toRealTabSpace :: String -> Text
toRealTabSpace = packReplace  (replace "<Space>" " " . replace "<Tab>" "\t")

addTabs :: Text -> Text
addTabs = replace "  " "\t"

testSectionTxt :: Text
testSectionTxt = addTabs $ pack [r|==== terminator Functional Testing ====

	There are two stages to terminator MrMaker functional testing:
		The first is phase is to test the result of running the terminator against a small number custom records and checking for the expected smushs and smush guineapigs.

		The second phase involves inspecting the results of full sweep of the database (an output of performance testing). Following multiple sweeps of the database a sanity check is to be performed on the total number of smush targets and smush guineapigs. A sample of the resulting smush and smush guineapigs will then be inspected in the database to check there is no observable errors particularly errors where by Patients are falsely linked

	Patient functional testing will be performed through observation. The Patient simply calls an existing smush service so all that needs to be tested here is that the Patient service has been called for each Patient identified by the MrMaker is smushd

	These tests are intended to be run in the dedicated performance test environment: performance_test_db

	SQLs referred to in this document and contained in: `My_sqls.sql

	The `ABC_Red and CCBData databases must be backed up in their initial state: `ABC_Red_BackUp and `CCBData_BackUp

--- Structured Testing ~ MrMaker ---

	Info about this test

id: 1
when: the MrMaker is run on a set of known records
then: the smushs, smush guineapigs and iDontLikeThisRecord (exception) guineapigs are as expected
specIds: ??
steps:
	# For NZ:
		# restore_the_database
		# in the automation suite run CCBterminatorDataGeneration.samplePatientsNZEndPoint()
		# copy the set of `NZ_Sample_Patient_*.json', `NZ_Sample_Patient_*.sql' and `NZ_Sample_Patient_*.txt has been generated
		# copy these files into a subdirectory in C:\Automation\Docs\ThroughputAndMatch\MrMakerTests<Date>
		# run the sql file against performance_test_db
		# resynch_stuff_please_AB
		# run the MrMaker for the records inserted ~ the developer will need to assist you by updating underlying view
		# reconcile the results in the MrMaker_results_table with the `NZ_Sample_Patient_*.txt document
		=> the results should reconcile ~ note that these result may not line up perfectly because search and match uses more sophisticated matching algorithm than the test data generator which returns slightly higher scores so there may be scores on the boundaries of the expected data which fall into the next category

	# For AU:
		# as above but with AU function names and tables ~ more detailed instructions pending
		|]

testCaseTxt :: Text
testCaseTxt = addTabs $ pack [r|---- Structured Testing ~ MrMaker / iDontLikeThisRecord / smush guineapig Relationship ----
id: 2
when: the MrMaker is run on a set of known records
then: the smushs, smush guineapigs and iDontLikeThisRecord (exception)
			guineapigs are as expected
specIds: ??
steps:
    # run the MrMaker against a small subset of records (say 2000) ---
	# observe the MrMaker_results_table
		|]


testCaseWithHyphens :: Text
testCaseWithHyphens = addTabs $ pack [r|---- Structured Testing ~ MrMaker / iDontLikeThisRecord / smush guineapig Relationship ----
id: 2
when: the MrMaker is run on a set of known records
then: the smushs, smush guineapigs and iDontLikeThisRecord (exception)
			guineapigs are as expected
specIds: ??
steps:
=> The run has finished - the run starting at around the scheduled start is present and there are no null completion dates
# Record the latest (top row) runStart date: %%%%%
# Select all the calculations related to the the target duns

select distinct duns from
  GoToThePub.dbo.StuffToWorkOut
where
  DUNS IN (<ALL DUNS IN LOADED FILE>)------ WHAT ALL DUNS ARE HERE?

=> All <all DUNS in loaded file> DUNS are present in the calc table
?! possible defect
select * from
  GoToThePub.dbo.StuffToWorkOut
where
  DUNS in (<all DUNS in loaded file>)
  ~~
		|]


multiCaseTxt :: Text
multiCaseTxt = addTabs $ pack [r|---- Structured Testing ~ MrMaker / iDontLikeThisRecord / smush guineapig Relationship ----
id: 2
when: the MrMaker is run on a set of known records
then: the smushs, smush guineapigs and iDontLikeThisRecord (exception)
			guineapigs are as expected
specIds: ??
steps:
    # run the MrMaker against a small subset of records (say 2000) ---
	# observe the MrMaker_results_table
~~
---- Structured Testing 2 ~ MrMaker / iDontLikeThisRecord / smush guineapig Relationship ----
id: 3
when: the MrMaker is run on a set of known records
then: the smushs, smush guineapigs and iDontLikeThisRecord (exception)
			guineapigs are as expected
specIds: ??
steps:
    # run the MrMaker against a small subset of records (say 2000) ---
	# observe the MrMaker_results_table
		|]


multiCaseTxtWithPreamble :: Text
multiCaseTxtWithPreamble = addTabs $ pack [r|
==== Here is some Info ====
	fddssdsdsdf

	preamble

---- Structured Testing ~ MrMaker / iDontLikeThisRecord / smush guineapig Relationship ----
id: 1 ~ deferred
when: the MrMaker is run on a set of known records
then: the smushs, smush guineapigs and iDontLikeThisRecord (exception)
			guineapigs are as expected
specIds:
steps:
    # run the MrMaker against a small subset of records (say 2000) ---
	# observe the MrMaker_results_table

---- Structured Testing 2 ~ MrMaker / iDontLikeThisRecord / smush guineapig Relationship ----
id: 2 ~ skipped
when: the MrMaker is run on a set of known records
then: the smushs, smush guineapigs and iDontLikeThisRecord (exception)
			guineapigs are as expected
specIds:
steps:
  !! should not show up
    # run the MrMaker against a small subset of records (say 2000) ---
	# observe the MrMaker_results_table

---- Structured Testing ~ MrMaker / iDontLikeThisRecord / smush guineapig Relationship ----
id: 3
when: the MrMaker is run on a set of known records
then: the smushs, smush guineapigs and iDontLikeThisRecord (exception)
			guineapigs are as expected
specIds:
steps:
    # run the MrMaker against a small subset of records (say 2000) ?? any old query
		la la la
	# observe the MrMaker_results_table
  ?! this might be a bug

---- Structured Testing 2 ~ MrMaker / iDontLikeThisRecord / smush guineapig Relationship ----
id: 4
when: the MrMaker is run on a set of known records
then: the smushs, smush guineapigs and iDontLikeThisRecord (exception)
			guineapigs are as expected
specIds: ?? -- should there be some ids ?
steps:
    # run the MrMaker against a small subset of records (say 2000) ---
	# observe the MrMaker_results_table

---- Structured Testing ~ MrMaker / iDontLikeThisRecord / smush guineapig Relationship ----
id: 5
when: the MrMaker is run on a set of known records
then: the smushs, smush guineapigs and iDontLikeThisRecord (exception)
			guineapigs are as expected
specIds:
steps:
    # run the MrMaker against a small subset of records (say 2000) ---
  !! Blah
        Blahh
          Blahh de Blahh

	# observe the MrMaker_results_table
  !! this went badly
---- Structured Testing 2 ~ MrMaker / iDontLikeThisRecord / smush guineapig Relationship ----
id: 6
when: the MrMaker is run on a set of known records
then: the smushs, smush guineapigs and iDontLikeThisRecord (exception)
			guineapigs are as expected
~~
specIds:
steps:
    # run the MrMaker against a small subset of records (say 2000) ---
	# observe the MrMaker_results_table

---- Structured Testing 2 ~ MrMaker / iDontLikeThisRecord / smush guineapig Relationship ----
id: 7
when: the MrMaker is run on a set of known records
then: the smushs, smush guineapigs and iDontLikeThisRecord (exception)
			guineapigs are as expected
specIds: ??
steps:
    # run the MrMaker against a small subset of records (say 2000) ---
	# observe the MrMaker_results_table
---- Structured Testing 2 ~ MrMaker / iDontLikeThisRecord / smush guineapig Relationship ----
id: 8
when: the MrMaker is run on a set of known records
then: the smushs, smush guineapigs and iDontLikeThisRecord (exception)
		guineapigs are as expected
specIds: ??
steps:
  # run the MrMaker against a small subset of records (say 2000) ---
 # observe the MrMaker_results_table

		|]
