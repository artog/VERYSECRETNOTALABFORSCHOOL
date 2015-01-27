package edu.gu.hajo.dict.core;

/**
 * A language
 *
 * @author hajo
 *
 */
public enum Language {

    /* IETF BCP 47 standard "Tags for Identifying Languages"
     * Must be in this order to be sorted correct
     */
    xx_XX("xx_XX"), // The undefined language (an extension)
    en_US("en_US"),
    sv_SV("sv_SV"),
    de_DE("de_DE"); // More to be added (not used)

    private final String bcp47;

    // A bit unnecessary because the string form
    // is default the same
    private Language(String bcp47) {
        this.bcp47 = bcp47;
    }

    public String getCountryCode() {
        return bcp47.split("[_]")[0];
    }

    public String getRegionCode() {
        return bcp47.split("[_]")[1];
    }

    @Override
    public String toString() {
        return bcp47;
    }
}
