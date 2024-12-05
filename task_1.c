#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct {
    char *county;
    char *state;
    float edu_bachelors;
    float edu_hs;
    float eth_ai;
    float eth_asian;
    float eth_black;
    float eth_hisp;
    float eth_nhpi;
    float eth_2more;
    float eth_white;
    float eth_white_non_hisp;
    int income_median;
    int income_percap;
    float income_poverty;
    int pop_2014;
    int active;
} CountyRecord;

typedef struct {
    int idx_county;
    int idx_state;
    int idx_edu_bachelors;
    int idx_edu_hs;
    int idx_eth_ai;
    int idx_eth_asian;
    int idx_eth_black;
    int idx_eth_hisp;
    int idx_eth_nhpi;
    int idx_eth_2more;
    int idx_eth_white;
    int idx_eth_white_non_hisp;
    int idx_inc_median;
    int idx_inc_percap;
    int idx_inc_pov;
    int idx_pop_2014;
} FieldIndices;

/* Safe string trimming function */
static void trim_whitespace(char *s) {
    char *start = s;
    while (isspace((unsigned char)*start)) start++;
    if (start != s) {
        memmove(s, start, strlen(start)+1);
    }

    char *end = s + strlen(s) - 1;
    while (end > s && isspace((unsigned char)*end)) end--;
    end[1] = '\0';
}

/* Convert string to float */
static int convert_to_float(const char *str, float *val) {
    if (!str || !*str) return -1;
    char *end;
    float f = strtof(str, &end);
    if (end == str) return -1; // no conversion
    *val = f;
    return 0;
}

/* Convert string to int */
static int convert_to_int(const char *str, int *val) {
    if (!str || !*str) return -1;
    char *end;
    long i = strtol(str, &end, 10);
    if (end == str) return -1; // no conversion
    *val = (int)i;
    return 0;
}

/* Find the index of a column by name in the already trimmed headers */
static int find_header_index(char **headers, int count, const char *name) {
    for (int i=0; i<count; i++) {
        if (strcmp(headers[i], name) == 0) {
            return i;
        }
    }
    return -1;
}

static int is_numeric_field(const char *f) {
    if (strcmp(f, "County") == 0 || strcmp(f, "State") == 0) {
        return 0;
    }
    return 1;
}

static int is_population_subfield(const char *f) {
    if (strncmp(f, "Education.", 10) == 0) return 1;
    if (strncmp(f, "Ethnicities.", 12) == 0) return 1;
    if (strcmp(f, "Income.Persons Below Poverty Level") == 0) return 1;
    return 0;
}

/* Parse a CSV line into a CountyRecord */
static int parse_csv_line(char *line, int line_num, FieldIndices *fi, CountyRecord *rec) {
    char *fields[200];
    int count = 0;
    char *tmp = line;
    char *token;
    while ((token = strsep(&tmp, ",")) != NULL) {
        // Just trim whitespace here; CSV fields may be quoted fields from the dataset
        trim_whitespace(token);
        // Remove surrounding quotes if present
        size_t len = strlen(token);
        if (len > 1 && token[0] == '"' && token[len-1] == '"') {
            token[len-1] = '\0';
            memmove(token, token+1, strlen(token)); 
        }
        fields[count++] = token;
    }

    const int required_indices[] = {
        fi->idx_county,
        fi->idx_state,
        fi->idx_edu_bachelors,
        fi->idx_edu_hs,
        fi->idx_eth_ai,
        fi->idx_eth_asian,
        fi->idx_eth_black,
        fi->idx_eth_hisp,
        fi->idx_eth_nhpi,
        fi->idx_eth_2more,
        fi->idx_eth_white,
        fi->idx_eth_white_non_hisp,
        fi->idx_inc_median,
        fi->idx_inc_percap,
        fi->idx_inc_pov,
        fi->idx_pop_2014
    };

    int num_required_fields = (int)(sizeof(required_indices)/sizeof(required_indices[0]));
    for (int i = 0; i < num_required_fields; i++) {
        if (required_indices[i] < 0 || required_indices[i] >= count) {
            return -1;  // Invalid field index
        }
    }

    rec->county = strdup(fields[fi->idx_county]);
    rec->state = strdup(fields[fi->idx_state]);

    if (convert_to_float(fields[fi->idx_edu_bachelors], &rec->edu_bachelors) < 0) return -1;
    if (convert_to_float(fields[fi->idx_edu_hs], &rec->edu_hs) < 0) return -1;
    if (convert_to_float(fields[fi->idx_eth_ai], &rec->eth_ai) < 0) return -1;
    if (convert_to_float(fields[fi->idx_eth_asian], &rec->eth_asian) < 0) return -1;
    if (convert_to_float(fields[fi->idx_eth_black], &rec->eth_black) < 0) return -1;
    if (convert_to_float(fields[fi->idx_eth_hisp], &rec->eth_hisp) < 0) return -1;
    if (convert_to_float(fields[fi->idx_eth_nhpi], &rec->eth_nhpi) < 0) return -1;
    if (convert_to_float(fields[fi->idx_eth_2more], &rec->eth_2more) < 0) return -1;
    if (convert_to_float(fields[fi->idx_eth_white], &rec->eth_white) < 0) return -1;
    if (convert_to_float(fields[fi->idx_eth_white_non_hisp], &rec->eth_white_non_hisp) < 0) return -1;
    if (convert_to_int(fields[fi->idx_inc_median], &rec->income_median) < 0) return -1;
    if (convert_to_int(fields[fi->idx_inc_percap], &rec->income_percap) < 0) return -1;
    if (convert_to_float(fields[fi->idx_inc_pov], &rec->income_poverty) < 0) return -1;
    if (convert_to_int(fields[fi->idx_pop_2014], &rec->pop_2014) < 0) return -1;

    rec->active = 1;
    return 0;
}

static CountyRecord *load_demographics(const char *filename, int *num_records) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        fprintf(stderr, "Error: Cannot open demographics file '%s'\n", filename);
        return NULL;
    }

    char line[10240];
    // Read header line
    if (!fgets(line, sizeof(line), fp)) {
        fprintf(stderr, "Error: Demographics file is empty.\n");
        fclose(fp);
        return NULL;
    }

    // Parse header
    char *headers[200];
    int hcount = 0;
    {
        char *tmp = line;
        char *token;
        while ((token = strsep(&tmp, ",")) != NULL) {
            trim_whitespace(token);
            // Remove surrounding quotes if present
            size_t len = strlen(token);
            if (len > 1 && token[0] == '"' && token[len-1] == '"') {
                token[len-1] = '\0';
                memmove(token, token+1, strlen(token));
            }
            headers[hcount++] = strdup(token);
        }
    }

    // Debug prints for headers
    /*
    printf("Found %d headers:\n", hcount);
    for (int i = 0; i < hcount; i++) {
        printf("Header %d: '%s'\n", i, headers[i]);
    }
    */

    FieldIndices fi;
    fi.idx_county = find_header_index(headers, hcount, "County");
    fi.idx_state = find_header_index(headers, hcount, "State");
    fi.idx_edu_bachelors = find_header_index(headers, hcount, "Education.Bachelor's Degree or Higher");
    fi.idx_edu_hs = find_header_index(headers, hcount, "Education.High School or Higher");
    fi.idx_eth_ai = find_header_index(headers, hcount, "Ethnicities.American Indian and Alaska Native Alone");
    fi.idx_eth_asian = find_header_index(headers, hcount, "Ethnicities.Asian Alone");
    fi.idx_eth_black = find_header_index(headers, hcount, "Ethnicities.Black Alone");
    fi.idx_eth_hisp = find_header_index(headers, hcount, "Ethnicities.Hispanic or Latino");
    fi.idx_eth_nhpi = find_header_index(headers, hcount, "Ethnicities.Native Hawaiian and Other Pacific Islander Alone");
    fi.idx_eth_2more = find_header_index(headers, hcount, "Ethnicities.Two or More Races");
    fi.idx_eth_white = find_header_index(headers, hcount, "Ethnicities.White Alone");
    fi.idx_eth_white_non_hisp = find_header_index(headers, hcount, "Ethnicities.White Alone not Hispanic or Latino");
    fi.idx_inc_median = find_header_index(headers, hcount, "Income.Median Household Income");
    fi.idx_inc_percap = find_header_index(headers, hcount, "Income.Per Capita Income");
    fi.idx_inc_pov = find_header_index(headers, hcount, "Income.Persons Below Poverty Level");
    fi.idx_pop_2014 = find_header_index(headers, hcount, "Population.2014 Population");

    // Check if any required field is missing
    int *fields_array = (int*)&fi;
    for (int i=0; i<16; i++) {
        if (fields_array[i] < 0) {
            fprintf(stderr, "Error: Missing required column in demographics file.\n");
            fclose(fp);
            return NULL;
        }
    }

    int capacity = 5000;
    CountyRecord *records = malloc(sizeof(CountyRecord)*capacity);
    int count = 0;
    int line_num = 1; // header is line 1
    while (fgets(line, sizeof(line), fp)) {
        line_num++;
        if (count >= capacity) {
            capacity *= 2;
            records = realloc(records, sizeof(CountyRecord)*capacity);
        }
        CountyRecord temp;
        if (parse_csv_line(line, line_num, &fi, &temp) == 0) {
            records[count++] = temp;
        } else {
            fprintf(stderr, "Error: Malformed line %d in demographics file. Skipping.\n", line_num);
        }
    }

    fclose(fp);

    *num_records = count;
    printf("%d records loaded\n", count);
    return records;
}


/* display: print all active records */
static void op_display(CountyRecord *records, int count) {
    for (int i=0; i<count; i++) {
        if (records[i].active) {
            printf("%s, %s\n", records[i].county, records[i].state);
            printf("        Population: %d\n", records[i].pop_2014);
            printf("        Education\n");
            printf("                >= High School: %f%%\n", records[i].edu_hs);
            printf("                >= Bachelor's: %f%%\n", records[i].edu_bachelors);
            printf("        Ethnicity Percentages\n");
            printf("                American Indian and Alaska Native: %f%%\n", records[i].eth_ai);
            printf("                Asian Alone: %f%%\n", records[i].eth_asian);
            printf("                Black Alone: %f%%\n", records[i].eth_black);
            printf("                Hispanic or Latino: %f%%\n", records[i].eth_hisp);
            printf("                Native Hawaiian and Other Pacific Islander Alone: %f%%\n", records[i].eth_nhpi);
            printf("                Two or More Races: %f%%\n", records[i].eth_2more);
            printf("                White Alone: %f%%\n", records[i].eth_white);
            printf("                White Alone, not Hispanic or Latino: %f%%\n", records[i].eth_white_non_hisp);
            printf("        Income\n");
            printf("                Median Household: %d\n", records[i].income_median);
            printf("                Per Capita: %d\n", records[i].income_percap);
            printf("                Below Poverty Level: %f%%\n", records[i].income_poverty);
            printf("\n");
        }
    }
}

/* filter-state:<state abbreviation> */
static void op_filter_state(CountyRecord *records, int count, const char *state_abbr) {
    int remain = 0;
    for (int i=0; i<count; i++) {
        if (records[i].active && strcmp(records[i].state, state_abbr) == 0) {
            remain++;
        } else {
            records[i].active = 0;
        }
    }
    printf("Filter: state == %s (%d entries)\n", state_abbr, remain);
}

/* filter:<field>:<ge/le>:<number> */
static void op_filter_numeric(CountyRecord *records, int count, const char *field, const char *op, float number) {
    int remain = 0;
    for (int i=0; i<count; i++) {
        if (!records[i].active) continue;
        float val;
        if (strcmp(field, "Education.Bachelor's Degree or Higher") == 0) val = records[i].edu_bachelors;
        else if (strcmp(field, "Education.High School or Higher") == 0) val = records[i].edu_hs;
        else if (strcmp(field, "Ethnicities.American Indian and Alaska Native Alone") == 0) val = records[i].eth_ai;
        else if (strcmp(field, "Ethnicities.Asian Alone") == 0) val = records[i].eth_asian;
        else if (strcmp(field, "Ethnicities.Black Alone") == 0) val = records[i].eth_black;
        else if (strcmp(field, "Ethnicities.Hispanic or Latino") == 0) val = records[i].eth_hisp;
        else if (strcmp(field, "Ethnicities.Native Hawaiian and Other Pacific Islander Alone") == 0) val = records[i].eth_nhpi;
        else if (strcmp(field, "Ethnicities.Two or More Races") == 0) val = records[i].eth_2more;
        else if (strcmp(field, "Ethnicities.White Alone") == 0) val = records[i].eth_white;
        else if (strcmp(field, "Ethnicities.White Alone, not Hispanic or Latino") == 0) val = records[i].eth_white_non_hisp;
        else if (strcmp(field, "Income.Median Household Income") == 0) val = (float)records[i].income_median;
        else if (strcmp(field, "Income.Per Capita Income") == 0) val = (float)records[i].income_percap;
        else if (strcmp(field, "Income.Persons Below Poverty Level") == 0) val = records[i].income_poverty;
        else if (strcmp(field, "Population.2014 Population") == 0) val = (float)records[i].pop_2014;
        else {
            // Field not found
            fprintf(stderr, "Warning: filter: field '%s' not found or not numeric.\n", field);
            return;
        }

        int keep = 0;
        if (strcmp(op, "ge") == 0) {
            if (val >= number) keep = 1;
        } else if (strcmp(op, "le") == 0) {
            if (val <= number) keep = 1;
        } else {
            fprintf(stderr, "Warning: filter: invalid operator '%s'.\n", op);
            return;
        }

        if (!keep) {
            records[i].active = 0;
        } else {
            remain++;
        }
    }

    printf("Filter: %s %s %f (%d entries)\n", field, op, number, remain);
}

/* population-total */
static void op_population_total(CountyRecord *records, int count) {
    long long total = 0;
    for (int i=0; i<count; i++) {
        if (records[i].active) {
            total += records[i].pop_2014;
        }
    }
    printf("2014 population: %lld\n", total);
}

/* population:<field> - compute total sub-population */
static void op_population_sub(CountyRecord *records, int count, const char *field) {
    double total = 0.0;
    for (int i=0; i<count; i++) {
        if (!records[i].active) continue;
        float val;
        if (strcmp(field, "Education.Bachelor's Degree or Higher") == 0) val = records[i].edu_bachelors;
        else if (strcmp(field, "Education.High School or Higher") == 0) val = records[i].edu_hs;
        else if (strcmp(field, "Ethnicities.American Indian and Alaska Native Alone") == 0) val = records[i].eth_ai;
        else if (strcmp(field, "Ethnicities.Asian Alone") == 0) val = records[i].eth_asian;
        else if (strcmp(field, "Ethnicities.Black Alone") == 0) val = records[i].eth_black;
        else if (strcmp(field, "Ethnicities.Hispanic or Latino") == 0) val = records[i].eth_hisp;
        else if (strcmp(field, "Ethnicities.Native Hawaiian and Other Pacific Islander Alone") == 0) val = records[i].eth_nhpi;
        else if (strcmp(field, "Ethnicities.Two or More Races") == 0) val = records[i].eth_2more;
        else if (strcmp(field, "Ethnicities.White Alone") == 0) val = records[i].eth_white;
        else if (strcmp(field, "Ethnicities.White Alone, not Hispanic or Latino") == 0) val = records[i].eth_white_non_hisp;
        else if (strcmp(field, "Income.Persons Below Poverty Level") == 0) val = records[i].income_poverty;
        else {
            fprintf(stderr, "Warning: population:<field>: invalid field '%s'\n", field);
            return;
        }

        double sub_pop = (double)records[i].pop_2014 * (val / 100.0);
        total += sub_pop;
    }
    printf("2014 %s population: %f\n", field, total);
}

/* percent:<field> */
static void op_percent_field(CountyRecord *records, int count, const char *field) {
    long long total_pop = 0;
    double sub_pop = 0.0;
    for (int i=0; i<count; i++) {
        if (!records[i].active) continue;
        total_pop += records[i].pop_2014;
    }

    if (total_pop == 0) {
        printf("2014 %s percentage: 0\n", field);
        return;
    }

    for (int i=0; i<count; i++) {
        if (!records[i].active) continue;
        float val;
        if (strcmp(field, "Education.Bachelor's Degree or Higher") == 0) val = records[i].edu_bachelors;
        else if (strcmp(field, "Education.High School or Higher") == 0) val = records[i].edu_hs;
        else if (strcmp(field, "Ethnicities.American Indian and Alaska Native Alone") == 0) val = records[i].eth_ai;
        else if (strcmp(field, "Ethnicities.Asian Alone") == 0) val = records[i].eth_asian;
        else if (strcmp(field, "Ethnicities.Black Alone") == 0) val = records[i].eth_black;
        else if (strcmp(field, "Ethnicities.Hispanic or Latino") == 0) val = records[i].eth_hisp;
        else if (strcmp(field, "Ethnicities.Native Hawaiian and Other Pacific Islander Alone") == 0) val = records[i].eth_nhpi;
        else if (strcmp(field, "Ethnicities.Two or More Races") == 0) val = records[i].eth_2more;
        else if (strcmp(field, "Ethnicities.White Alone") == 0) val = records[i].eth_white;
        else if (strcmp(field, "Ethnicities.White Alone, not Hispanic or Latino") == 0) val = records[i].eth_white_non_hisp;
        else if (strcmp(field, "Income.Persons Below Poverty Level") == 0) val = records[i].income_poverty;
        else {
            fprintf(stderr, "Warning: percent:<field>: invalid field '%s'\n", field);
            return;
        }
        sub_pop += (double)records[i].pop_2014 * (val / 100.0);
    }

    double percentage = (sub_pop / (double)total_pop)*100.0;
    printf("2014 %s percentage: %f\n", field, percentage);
}

static void process_operation_line(char *line, int line_num, CountyRecord *records, int count) {
    char *p = line;
    while (isspace((unsigned char)*p)) p++;
    if (*p == '\0') {
        return; // blank line
    }

    char op_line[1024];
    strcpy(op_line, p);
    // Remove trailing whitespace
    for (int i=(int)strlen(op_line)-1; i>=0 && isspace((unsigned char)op_line[i]); i--) {
        op_line[i] = '\0';
    }

    char *saveptr;
    char *op = strtok_r(op_line, ":", &saveptr);
    if (!op) {
        fprintf(stderr, "Error: Malformed operation line %d\n", line_num);
        return;
    }

    if (strcmp(op, "display") == 0) {
        op_display(records, count);
    } else if (strcmp(op, "filter-state") == 0) {
        char *state = strtok_r(NULL, ":", &saveptr);
        if (!state) {
            fprintf(stderr, "Error: Malformed operation line %d: filter-state requires a state code.\n", line_num);
            return;
        }
        op_filter_state(records, count, state);
    } else if (strcmp(op, "filter") == 0) {
        char *field = strtok_r(NULL, ":", &saveptr);
        char *cmp = strtok_r(NULL, ":", &saveptr);
        char *num_str = strtok_r(NULL, ":", &saveptr);
        if (!field || !cmp || !num_str) {
            fprintf(stderr, "Error: Malformed operation line %d: filter requires field:op:number.\n", line_num);
            return;
        }
        if (!is_numeric_field(field)) {
            fprintf(stderr, "Error: filter field '%s' is not numeric.\n", field);
            return;
        }
        float number;
        if (convert_to_float(num_str, &number) < 0) {
            fprintf(stderr, "Error: filter number '%s' invalid on line %d.\n", num_str, line_num);
            return;
        }
        if ((strcmp(cmp, "ge") != 0) && (strcmp(cmp, "le") != 0)) {
            fprintf(stderr, "Error: filter comparison '%s' invalid on line %d.\n", cmp, line_num);
            return;
        }
        op_filter_numeric(records, count, field, cmp, number);
    } else if (strcmp(op, "population-total") == 0) {
        op_population_total(records, count);
    } else if (strcmp(op, "population") == 0) {
        char *field = strtok_r(NULL, ":", &saveptr);
        if (!field) {
            fprintf(stderr, "Error: Malformed population operation at line %d.\n", line_num);
            return;
        }
        if (!is_population_subfield(field)) {
            fprintf(stderr, "Error: population field '%s' not supported.\n", field);
            return;
        }
        op_population_sub(records, count, field);
    } else if (strcmp(op, "percent") == 0) {
        char *field = strtok_r(NULL, ":", &saveptr);
        if (!field) {
            fprintf(stderr, "Error: Malformed percent operation at line %d.\n", line_num);
            return;
        }
        if (!is_population_subfield(field)) {
            fprintf(stderr, "Error: percent field '%s' not supported.\n", field);
            return;
        }
        op_percent_field(records, count, field);
    } else {
        fprintf(stderr, "Error: Unrecognized operation '%s' on line %d.\n", op, line_num);
    }
}

static void process_operations(const char *filename, CountyRecord *records, int count) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        fprintf(stderr, "Error: Cannot open operations file '%s'\n", filename);
        return;
    }

    char line[1024];
    int line_num = 0;
    while (fgets(line, sizeof(line), fp)) {
        line_num++;
        process_operation_line(line, line_num, records, count);
    }

    fclose(fp);
}

static void free_records(CountyRecord *records, int count) {
    for (int i=0; i<count; i++) {
        free(records[i].county);
        free(records[i].state);
    }
    free(records);
}

int main(int argc, char *argv[]) {
    if (argc < 3) {
        fprintf(stderr, "Call with 2 arguments: <demographics_file> <operations_file>\n");
        return 1;
    }

    const char *dem_file = argv[1];
    const char *ops_file = argv[2];

    FILE *fp = fopen(dem_file, "r");
    if (!fp) {
        fprintf(stderr, "Error: Cannot open demographics file '%s'\n", dem_file);
        return 1;
    }
    fclose(fp);

    fp = fopen(ops_file, "r");
    if (!fp) {
        fprintf(stderr, "Error: Cannot open operations file '%s'\n", ops_file);
        return 1;
    }
    fclose(fp);

    int count;
    CountyRecord *records = load_demographics(dem_file, &count);
    if (!records) {
        return 1;
    }

    process_operations(ops_file, records, count);

    free_records(records, count);
    return 0;
}
