#!/usr/bin/perl
use strict;
use warnings;
use 5.30.0;
no autovivification;
binmode STDOUT, ":utf8";
use utf8;
use Data::Printer;
use Scalar::Util qw(looks_like_number);

my %data = ();

open my $in, '<:utf8', 'Provisional Mortality Statistics, 2018 through Last Week.txt';
while (<$in>) {
	chomp $_;
	last if $_ eq '"---"';
	$_ =~ s/\"//g;
	my @elems = split '	', $_;
	# p@elems;
	my $county = $elems[1] // die;
	next if $county eq 'Residence County';
	my $county_code = $elems[2] // die;
	my $year = $elems[3] // die;
	$year =~ s/ \(provisional\)//;
	$year =~ s/ \(provisional and partial\)//;
	$year =~ s/ //g;
	my $deaths = $elems[5] // die;
	die "[$deaths]" unless looks_like_number($deaths);
	$data{$county_code}->{$year}->{'deaths'} = $deaths;
}
close $in;

open $in, '<:utf8', 'Multiple Cause of Death, 1999-2020.txt';
while (<$in>) {
	chomp $_;
	last if $_ eq '"---"';
	$_ =~ s/\"//g;
	my @elems = split '	', $_;
	my $county = $elems[1] // die;
	next if $county eq 'County';
	my $county_code = $elems[2] // die;
	my $year = $elems[3] // die;
	$year =~ s/ //g;
	next if $year < 2015;
	my $deaths = $elems[5] // die;
	my $population = $elems[6] // die;
	$data{$county_code}->{$year}->{'deaths'} = $deaths;
	$data{$county_code}->{$year}->{'population'} = $population;

}
close $in;

my ($cpt, $total) = (0, 0);
my %covid_dose_1  = ();
open $in, '<:utf8', 'COVID-19_Vaccinations_in_the_United_States_County_20241231.csv';
while (<$in>) {
	chomp $_;
	$cpt++;
	$total++;
	if ($cpt == 1000) {
		$cpt = 0;
		STDOUT->printflush("\rLoading vaccination data - [$total]");
	}
	my @elems = split ',', $_;
	my $date = $elems[0] // die;
	next if $date eq 'Date';
	my $county_code = $elems[1] // die;
	next if $county_code eq 'UNK';
	my $dose_1_percent = $elems[7] // die;
	my ($month, $day, $year) = split '\/', $date;
	my $yyyymmdd_date = "$year-$month-$day";
	my $compdate = $yyyymmdd_date;
	$compdate =~ s/\D//g;
	if (looks_like_number $dose_1_percent) {
		$covid_dose_1{$county_code}->{$compdate}->{'date'} = $yyyymmdd_date;
		$covid_dose_1{$county_code}->{$compdate}->{'dose_1_percent'} = $dose_1_percent;
	}
	# say "$date - $county_code - $dose_1_percent";
}
close $in;
say "";

open my $out, '>:utf8', '65_plus_deaths_by_counties.csv';
say $out "county_code,year,deaths,population_year,population,rate";
open my $out_2, '>:utf8', 'excluded_counties.csv';
say $out_2 "county_code,motive";
my $no_recent_pop_data = 0;
for my $county_code (sort keys %data) {

	# Verifying population data.
	my ($population, $population_year);
	my $years = keys %{$data{$county_code}};
	if ($years != 10) {
		if (!exists $data{$county_code}->{'2015'} || (!exists $data{$county_code}->{'2019'} && !exists $data{$county_code}->{'2020'})) {
			say $out_2 "$county_code,Insufficient yearly data";
			# say "$county_code - $years skipped";
			# p$data{$county_code};
			next;
		}
	}

	# Verifying covid doses data.
	my ($has_2020, $has_2021, $has_2022, $has_2023) = (0, 0, 0, 0);
	my %rates = ();
	my ($rate_2020, $rate_2021, $rate_2022, $rate_2023) = (0, 0, 0, 0);
	for my $compdate (sort{$a <=> $b} keys %{$covid_dose_1{$county_code}}) {
		if ($compdate =~ /^202012*/ || $compdate =~ /^202101*/ || $compdate =~ /^202102*/) {
			$has_2020 = 1;
			$rates{2020} = $covid_dose_1{$county_code}->{$compdate}->{'dose_1_percent'} // die;
		}
		if ($compdate =~ /^202112*/ || $compdate =~ /^202201*/) {
			$has_2021 = 1;
			$rates{2021} = $covid_dose_1{$county_code}->{$compdate}->{'dose_1_percent'} // die;
		}
		if ($compdate =~ /^202212*/ || $compdate =~ /^202301*/) {
			$has_2022 = 1;
			$rates{2022} = $covid_dose_1{$county_code}->{$compdate}->{'dose_1_percent'} // die;
		}
		if ($compdate =~ /^202312*/ || $compdate =~ /^202401*/) {
			$has_2023 = 1;
			$rates{2023} = $covid_dose_1{$county_code}->{$compdate}->{'dose_1_percent'} // die;
		}
	}
	if ($has_2020 != 1 || $has_2021 != 1 || $has_2022 != 1) {
		# say "$county_code : $has_2020, $has_2021, $has_2022, $has_2023";
		say $out_2 "$county_code,Insufficient COVID dose 1 data";
		# say "$county_code - $years skipped";
		# p$data{$county_code};
		next;
	}
	if (!$has_2023) {
		$rates{2023} = $rates{2022};
	}
	# p%rates;
	# die;
	# say "$county_code - $years";
	for my $year (sort{$a <=> $b} keys %{$data{$county_code}}) {
		my $deaths = $data{$county_code}->{$year}->{'deaths'} // die "$county_code -> $year";
		if (exists $data{$county_code}->{$year}->{'population'}) {
			my $tmp = $data{$county_code}->{$year}->{'population'} // die;
			if ($tmp) {
				$population = $data{$county_code}->{$year}->{'population'} // die;
				$population_year = $year;
			}
		}
		die "$county_code -> $year" unless $population;
		my $year_minus_1 = $year - 1;
		my $rate = $rates{$year_minus_1} // '';
		if ($year > 2020 && ($population_year ne "2020" && $population_year ne "2019")) {
			# say "[$county_code] - [$population_year]";
			$no_recent_pop_data++;
			die;
		}
		say $out "$county_code,$year,$deaths,$population_year,$population,$rate";
	}
}
close $out;
close $out_2;
say "No recent population data : $no_recent_pop_data";
