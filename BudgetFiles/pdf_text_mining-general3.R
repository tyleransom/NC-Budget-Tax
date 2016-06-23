rm(list = ls())
cat("\014") 

# Script to install and run tabulizer to extract tables from PDFs 

# packages necessary to install and run tabulizer. I had to install a different version of Java, which you may have to do as well. 
# require(devtools)
require(rJava)
require(tabulizer)

# if(!require("ghit")){
#    install.packages("ghit")
# }

# on 64-bit Windows
# ghit::install_github(c("leeper/tabulizerjars", "leeper/tabulizer"), INSTALL_opts = "--no-multiarch")
# elsewhere

listofnames <- list(
"2015_17/PublicEducation.pdf",
"2015_17/NCCommunityCollegeSystem.pdf",
"2015_17/UniversityOfNorthCarolinaGeneralAdministration.pdf",
"2015_17/UNCChapelHill.pdf",
"2015_17/NorthCarolinaStateUniversity.pdf",
"2015_17/UNCGreensboro.pdf",
"2015_17/UNCCharlotte.pdf",
"2015_17/UNCAsheville.pdf",
"2015_17/UNCWilmington.pdf",
"2015_17/EastCarolinaUniversity.pdf",
"2015_17/NCA&TUniversity.pdf",
"2015_17/WesternCarolinaUniversity.pdf",
"2015_17/AppalachianStateUniversity.pdf",
"2015_17/UNCPembroke.pdf",
"2015_17/WinstonSalemStateUniversity.pdf",
"2015_17/ElizabethCityStateUniversity.pdf",
"2015_17/FayettevilleStateUniversity.pdf",
"2015_17/NCCentralUniversity.pdf",
"2015_17/UNCSchoolOftheArts.pdf",
"2015_17/NCSchoolOfScienceandMathematics.pdf",
"2015_17/GeneralAssembly.pdf",
"2015_17/OfficeOftheGovernor.pdf",
"2015_17/OfficeOfStateBudgetandManagement.pdf",
"2015_17/DepartmentOfInformationTechnology.pdf",
"2015_17/NCHousingFinanceAgency.pdf",
"2015_17/DepartmentOfMilitaryandVeteransAffairs.pdf",
"2015_17/OfficeOftheLieutenantGovernor.pdf",
"2015_17/DepartmentOfSecretaryOfState.pdf",
"2015_17/OfficeOftheStateAuditor.pdf",
"2015_17/DepartmentOfStateTreasurer.pdf",
"2015_17/DepartmentOfInsurance.pdf",
"2015_17/DepartmentOfAdministration.pdf",
"2015_17/OfficeOftheStateController.pdf",
"2015_17/DepartmentOfRevenue.pdf",
"2015_17/StateBoardOfElections.pdf",
"2015_17/OfficeOfAdministrativeHearings.pdf",
"2015_17/NCStateBoardOfBarberExaminers.pdf",
"2015_17/NCStateBoardOfCosmeticArtExaminers.pdf",
"2015_17/NCStateBoardOfOpticians.pdf",
"2015_17/NCStatePsychologyBoard.pdf",
"2015_17/NCStateAuctioneerLicensingBoard.pdf",
"2015_17/NCStateBoardOfElectrolysisExaminers.pdf",
"2015_17/DivisionOfCentralManagementandSupport.pdf",
"2015_17/AgingandAdultServices.pdf",
"2015_17/ChildDevelopmentandEarlyEducation.pdf",
"2015_17/PublicHealth.pdf",
"2015_17/SocialServices.pdf",
"2015_17/MedicalAssistanceNCHealthChoice.pdf",
"2015_17/ServicesfortheBlindDeafandHardOfHearing.pdf",
"2015_17/MentalHealthDevelopmentalDisabilitiesSubstanceAbuseServices.pdf",
"2015_17/HealthServiceRegulation.pdf",
"2015_17/VocationalRehabilitation.pdf",
"2015_17/JudicialBranch.pdf",
"2015_17/JudicialBranchIndigentDefense.pdf",
"2015_17/DepartmentOfJustice.pdf",
"2015_17/DepartmentOfPublicSafety.pdf",
"2015_17/DepartmentOfAgricultureandConsumerServices.pdf",
"2015_17/DepartmentOfLabor.pdf",
"2015_17/DepartmentOfNaturalandCulturalResources.pdf",
"2015_17/DepartmentOfEnvironmentalQuality.pdf",
"2015_17/WildlifeResourcesCommission.pdf",
"2015_17/DepartmentOfCommerce.pdf",
"2015_17/NCEducationLottery.pdf",
"2015_17/Transportation.pdf",
"2015_17/StatewideReservesDebtServiceandOtherAdjustments.pdf"
)

# tell it where the PDF is
file <- listofnames[[1]]

a <- get_n_pages(file=file)
# extract tables from the PDF
Genout1 <- as.data.frame(extract_tables(file, pages = 2, guess = FALSE, method = "data.frame", columns = list(c(390, 475))),stringsAsFactors=FALSE)
for (i in 3:a) {
	try({
	Out <- as.data.frame(extract_tables(file, pages = i, guess = FALSE, method = "data.frame", columns = list(c(390, 475))),stringsAsFactors=FALSE)
	names(Out) <- names(Genout1)
	Genout1 <- rbind(Genout1,Out)
	})
}

# To put all in a loop:
# for (k in 1:length(listofnames)) {
	# vol{k}2015 <- generator(listofnames[[k]])
# }
