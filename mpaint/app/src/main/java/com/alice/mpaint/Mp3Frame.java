package com.alice.mpaint;

import android.util.Log;

/**
 * Created by alice on 2015/10/4.
 */
public class Mp3Frame {

    private int bitRateValue;
    private int bitRate;
    private int sampleRate;
    private int freq;
    private boolean padNoCRC;
    private int version;
    private int layer;
    private int dataLength;
    byte [] data;

    public void Mp3Frame(){

    }

    public void setBitRate(int bitRate) {
        this.bitRate = bitRate;
    }

    public void setFreq(int freq) {
        this.freq = freq;
    }

    public void setVersion(int version) {
        this.version = version;
    }

    public void setLayer(int layer) {
        this.layer = layer;
    }

    public void setData(byte data, int position) {
        this.data[position] = data;
    }

    public int getBitRate() {

        return bitRate;
    }

    public int getFreq() {
        return freq;
    }

    public boolean isPadNoCRC() {
        return padNoCRC;
    }

    public int getVersion() {
        return version;
    }

    public int getLayer() {
        return layer;
    }

    public byte[] getData() {
        return data;
    }

    public int getDataLength() {
        return dataLength;
    }

    public Mp3Frame(int version, int layer, boolean padNoCRC, int bitRate, int freq) {
        this.version = version;
        this.layer = layer;
        this.padNoCRC = padNoCRC;
        this.bitRate = bitRate;
        this.freq = freq;
        Log.d("alice_debug", "I am in the Mp3Frame");
        CalSampleRate(freq);
        CalBitRateValue(bitRate);
        CalDataLength(this.bitRateValue, this.sampleRate, padNoCRC);
        data = new byte[dataLength];
        Log.d("alice_debug", "dataLength is " + dataLength);
    }

    private void CalDataLength(int bitRateValue, int sampleRate, boolean  padNoCRC) {
        int frameSize = (int)Math.ceil(144.0*bitRateValue/sampleRate);
        if(!padNoCRC){
            frameSize = frameSize + 1;
        }
        this.dataLength = frameSize - 4;
    }

    private void CalBitRateValue(int bitRate) {
        switch(bitRate){
            case 1:
                this.bitRateValue = 32000;
                break;
            case 2:
                this.bitRateValue = 40000;
                break;
            case 3:
                this.bitRateValue = 48000;
                break;
            case 4:
                this.bitRateValue = 56000;
                break;
            case 5:
                this.bitRateValue = 64000;
                break;
            case 6:
                this.bitRateValue = 80000;
                break;
            case 7:
                this.bitRateValue = 96000;
                break;
            case 8:
                this.bitRateValue = 112000;
                break;
            case 9:
                this.bitRateValue = 128000;
                break;
            case 10:
                this.bitRateValue = 160000;
                break;
            case 11:
                this.bitRateValue = 192000;
                break;
            case 12:
                this.bitRateValue = 224000;
                break;
            case 13:
                this.bitRateValue = 256000;
                break;
            case 14:
                this.bitRateValue = 320000;
                break;
            default:
                this.bitRateValue = 128000;
                break;

        }
    }

    private void CalSampleRate(int freq) {
        switch (freq)
        {
            case 0:
                this.sampleRate = 44100;
                break;
            case 1:
                this.sampleRate = 38000;
                break;
            case 2:
                this.sampleRate = 32000;
                break;
            case 3:
            default:
                this.sampleRate = 44100;
                break;
        }

    }


}
