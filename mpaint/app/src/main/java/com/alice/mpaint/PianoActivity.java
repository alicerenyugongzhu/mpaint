package com.alice.mpaint;

import android.app.Activity;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.media.AudioManager;
import android.media.SoundPool;
import android.os.Bundle;
import android.util.AttributeSet;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;

import java.util.HashMap;

/**
 * Created by alice on 2015/10/25.
 */
public class PianoActivity extends Activity {

    //Handler
    SoundPool sp = null;
    HashMap<String, Integer> soundMap;
    PianoPaintView ppv= null;
    Canvas wholeCanvas = null;
    //Constant
    int SOUND_NUM = 7;
    int SOUND_QT = 5;

    @Override
    public void onCreate(Bundle savedInstance){
        super.onCreate(savedInstance);

        setContentView(R.layout.acttivity_piano);

        DisplayMetrics dm =getResources().getDisplayMetrics();
        getWindowManager().getDefaultDisplay().getMetrics(dm);


        ///SoundPool
        sp = new SoundPool(SOUND_NUM, AudioManager.STREAM_SYSTEM, SOUND_QT);
        soundMap = new HashMap<String, Integer>();
        soundMap.put("Do", sp.load(this, R.raw.c4, 1));
        soundMap.put("Re", sp.load(this, R.raw.d4, 1));
        soundMap.put("Mi", sp.load(this, R.raw.e4, 1));
        soundMap.put("Fa", sp.load(this, R.raw.f4, 1));
        soundMap.put("So", sp.load(this, R.raw.g4, 1));
        soundMap.put("La", sp.load(this, R.raw.a4, 1));
        soundMap.put("Xi", sp.load(this, R.raw.b4, 1));

        ImageButton buttonUp = (ImageButton)findViewById(R.id.up_tone);
        ViewGroup.LayoutParams params = buttonUp.getLayoutParams();
        params.width = dm.widthPixels/9;
        Bitmap bitmap = BitmapFactory.decodeResource(getResources(), R.drawable.up);
        int bwidth = bitmap.getWidth();
        int bHeight = bitmap.getHeight();
        params.height = (dm.widthPixels*bHeight)/(bwidth * 9);
        buttonUp.setLayoutParams(params);
        ImageButton buttonDown = (ImageButton)findViewById(R.id.down_tone);
        buttonDown.setLayoutParams(params);
        ImageButton buttonDo = (ImageButton)findViewById(R.id.tone_C);
        buttonDo.setLayoutParams(params);
        ImageButton buttonRe = (ImageButton)findViewById(R.id.tone_D);
        buttonRe.setLayoutParams(params);
        ImageButton buttonMi = (ImageButton)findViewById(R.id.tone_E);
        buttonMi.setLayoutParams(params);
        ImageButton buttonFa = (ImageButton)findViewById(R.id.tone_F);
        buttonFa.setLayoutParams(params);
        ImageButton buttonSo = (ImageButton)findViewById(R.id.tone_G);
        buttonSo.setLayoutParams(params);
        ImageButton buttonLa = (ImageButton)findViewById(R.id.tone_A);
        buttonLa.setLayoutParams(params);
        ImageButton buttonXi = (ImageButton)findViewById(R.id.tone_B);
        buttonXi.setLayoutParams(params);

        ppv = (PianoPaintView)findViewById(R.id.piano_draw);
        ppv.ConfigView(dm.widthPixels, dm.heightPixels);

        //Button response
        View.OnClickListener upRsp = new View.OnClickListener(){

            @Override
            public void onClick(View v) {

            }
        };
        buttonUp.setOnClickListener(upRsp);

        View.OnClickListener downRsp = new View.OnClickListener(){

            @Override
            public void onClick(View v) {

            }
        };
        buttonDown.setOnClickListener(downRsp);

        View.OnClickListener doRsp = new View.OnClickListener(){

            @Override
            public void onClick(View v) {
                sp.play(soundMap.get("Do"),1, 1, 0, 0, 1);
                ppv.setTone("Do");
            }
        };
        buttonDo.setOnClickListener(doRsp);

        View.OnClickListener reRsp = new View.OnClickListener(){

            @Override
            public void onClick(View v) {
                sp.play(soundMap.get("Re"), 1, 1, 0, 0, 1);
                ppv.setTone("Re");
            }
        };
        buttonRe.setOnClickListener(reRsp);

        View.OnClickListener miRsp = new View.OnClickListener(){

            @Override
            public void onClick(View v) {
                sp.play(soundMap.get("Mi"),1, 1, 0, 0, 1);
                ppv.setTone("Mi");
            }
        };
        buttonMi.setOnClickListener(miRsp);

        View.OnClickListener faRsp = new View.OnClickListener(){

            @Override
            public void onClick(View v) {
                sp.play(soundMap.get("Fa"),1, 1, 0, 0, 1);
                ppv.setTone("Fa");
            }
        };
        buttonFa.setOnClickListener(faRsp);

        View.OnClickListener soRsp = new View.OnClickListener(){

            @Override
            public void onClick(View v) {
                sp.play(soundMap.get("So"),1, 1, 0, 0, 1);
                ppv.setTone("So");
            }
        };
        buttonSo.setOnClickListener(soRsp);

        View.OnClickListener laRsp = new View.OnClickListener(){

            @Override
            public void onClick(View v) {
                sp.play(soundMap.get("La"),1, 1, 0, 0, 1);
                ppv.setTone("La");
            }
        };
        buttonLa.setOnClickListener(laRsp);

        View.OnClickListener xiRsp = new View.OnClickListener(){

            @Override
            public void onClick(View v) {
                sp.play(soundMap.get("Xi"),1, 1, 0, 0, 1);
                ppv.setTone("Xi");
            }
        };
        buttonXi.setOnClickListener(xiRsp);


    }


}
