///////////////////////////////////////////////
// MKGlowSystem								 //
//											 //
// Created by Michael Kremmel on 23.12.2014  //
// Copyright © 2015 All rights reserved.     //
///////////////////////////////////////////////


using UnityEngine;
using System.Collections;
using MKGlowSystem;

public class MK_DemoControl : MonoBehaviour 
{
	private MKGlow mkGlow;
	private GUIStyle bStyle;
	private GUIStyle r2bStyle;
	public GUISkin skin;
	private int currentRoom = 0;
	private int currentRoom2Texture = 0;
	private int currentRoom1Object = 0;
	private int currentRoom1Texture = 0;
	private int currentRoom1GlowColor = 0;
	private int currentRoom1GlowTexColor = 2;
	private int currentRoom0GlowColor = 0;
	private int currentRoom1Shader = 0;
	public Shader[] room1Shaders = new Shader[3];
	public Cubemap cm;
	private float room1RimP = 3.0f;
	private int currentRimColor = 0;

	private float room1GlowIntensity = 0.75f;
	private float room1GlowTextureStrength = 1;
	private float room1GlowWidth = 0;

	public Texture[] room2Tex = new Texture[2];
	public GameObject[] room2Objects = new GameObject[10];
	public GameObject[] room1Objects = new GameObject[2];

	void Awake () 
	{
		mkGlow = this.GetComponent<MKGlow>();
		InitGlowSystem();
		skin.horizontalSlider.fixedHeight = 25f;
	}

	void Update () 
	{
		ManageRoom();
	}

	private void InitRoom1()
	{
		room1GlowTextureStrength = 1.0f;
		room1GlowIntensity = 1.0f;
		room1GlowWidth = 0.0f;
		currentRoom1GlowColor = 0;
		currentRoom1Object = 0;
		currentRoom1GlowTexColor = 0;
		currentRoom1Shader = 0;
		room1RimP = 3.0f;
		currentRimColor = 0;
	}

	private void InitRoom2()
	{
		currentRoom2Texture = 0;
	}

	private void InitGlowSystem()
	{
		mkGlow.BlurIterations = 5;
		mkGlow.BlurOffset = 0.25f;
		mkGlow.Samples = 4;
		mkGlow.GlowIntensity = 0.3f;
		mkGlow.BlurSpread = 0.25f;

		mkGlow.GlowType = MKGlowType.Selective;
		mkGlow.GlowQuality = MKGlowQuality.High;
		currentRoom0GlowColor = 0;
	}

	private void CreateMatrix(int depth, float nativeWidth, float nativeHeight)
	{
		float rx = Screen.width / nativeWidth;
		float ry = Screen.height / nativeHeight;
		
		GUI.matrix = Matrix4x4.TRS (new Vector3(0, 0, 1), Quaternion.identity, new Vector3 (rx, ry, depth));
	}

	private void OnGUI()
	{
		if(bStyle == null)
			bStyle = new GUIStyle (GUI.skin.button);

		if(r2bStyle == null)
			r2bStyle = new GUIStyle ();

		bStyle.fontSize = 40;
		r2bStyle.fontSize = 40;
		r2bStyle.normal.textColor = Color.white;
		CreateMatrix(1, 1920f, 1080f);
		SwitchRoom();

		if(currentRoom == 1)
			ManageRoom1();
		else if(currentRoom == 2)
			ManageRoom2();
		else
			ManageRoom0();
	}

	private void ManageRoom()
	{
		Quaternion quaternion = new Quaternion();

		if(currentRoom == 1)
			quaternion.eulerAngles = new Vector3(10,120,0);
		else if(currentRoom == 2)
			quaternion.eulerAngles = new Vector3(10,240,0);
		else
			quaternion.eulerAngles = new Vector3(10,0,0);

		this.transform.rotation = quaternion;
	}

	private void SwitchRoom()
	{
		if(GUI.Button(new Rect(835,950, 250,100), "Next Room", bStyle))
			currentRoom++;

		if(currentRoom > 2)
			currentRoom = 0;
	}

	private void ManageRoom0()
	{
		InitRoom1 ();
		InitRoom2 ();
		GUI.skin = skin;
		if(mkGlow.GlowType == MKGlowType.Selective)
		{
			if(GUI.Button(new Rect(25, 900, 400, 50), "Switch Glowmode", bStyle))
				mkGlow.GlowType = MKGlowType.Fullscreen;
		}
		else
		{
			if(GUI.Button(new Rect(25, 900, 400, 50), "Switch Glowmode", bStyle))
				mkGlow.GlowType = MKGlowType.Selective;
		}

		if(GUI.Button(new Rect(25, 800, 400, 50), "Switch Glowquality", bStyle))
		{
			if(mkGlow.GlowQuality == MKGlowQuality.High)
				mkGlow.GlowQuality = MKGlowQuality.Low;
			else
				mkGlow.GlowQuality = MKGlowQuality.High;
		}

		if(mkGlow.GlowQuality == MKGlowQuality.High)
			GUI.Label(new Rect(1500, 450, 410, 50), "Glowquality: High", r2bStyle);
		else
			GUI.Label(new Rect(1500, 450, 410, 50), "Glowquality: Low", r2bStyle);

		if(mkGlow.GlowType == MKGlowType.Selective)
			GUI.Label(new Rect(1500, 500, 410, 50), "GlowMode: Selective", r2bStyle);
		else
			GUI.Label(new Rect(1500, 500, 410, 50), "GlowMode: Fullscreen", r2bStyle);

		if(mkGlow.GlowType == MKGlowType.Fullscreen)
		{
			if(GUI.Button(new Rect(1500, 400, 400, 50), "Switch Color", bStyle))
			{
				currentRoom0GlowColor++;
				if(currentRoom0GlowColor > 4)
					currentRoom0GlowColor =0;
			}
			Color[] colors = new Color[5]{Color.white, Color.red,Color.cyan, Color.yellow, Color.green};
			colors[currentRoom0GlowColor].a = 0;
			mkGlow.FullScreenGlowTint = colors[currentRoom0GlowColor];
		}

		mkGlow.BlurSpread = GUI.HorizontalSlider(new Rect(1500, 650, 400, 100), mkGlow.BlurSpread, 0.2f, 1.0f);
		GUI.Label(new Rect(1500, 600, 300, 50), "Blur Spread", r2bStyle);
		mkGlow.BlurSpread = GUI.HorizontalSlider(new Rect(1500, 650, 400, 100), mkGlow.BlurSpread, 0.2f, 1.0f);
		GUI.Label(new Rect(1500, 700, 300, 50), "Blur Offset", r2bStyle);
		mkGlow.BlurOffset = GUI.HorizontalSlider(new Rect(1500, 750, 400, 100), mkGlow.BlurOffset, 0.0f, 1.0f);
		GUI.Label(new Rect(1500, 800, 300, 50), "Samples", r2bStyle);
		mkGlow.Samples = Mathf.RoundToInt(GUI.HorizontalSlider(new Rect(1500, 850, 400, 100), mkGlow.Samples, 1, 16));
		GUI.Label(new Rect(1500, 900, 300, 50), "Blur Iterations", r2bStyle);
		mkGlow.BlurIterations = Mathf.RoundToInt(GUI.HorizontalSlider(new Rect(1500, 950, 400, 100), mkGlow.BlurIterations, 0, 11));
		GUI.Label(new Rect(1500, 1000, 300, 50), "Glow Intensity", r2bStyle);
		mkGlow.GlowIntensity = GUI.HorizontalSlider(new Rect(1500, 1050, 400, 100), mkGlow.GlowIntensity, 0.0f, 1f);
	}
	private void ManageRoom1()
	{
		InitRoom2();
		InitGlowSystem ();
		Color[] colors = new Color[5]{Color.white, Color.red,Color.cyan, Color.yellow, Color.green};
		if(GUI.Button(new Rect(150, 860, 400, 50), "Switch Shader", bStyle))
		{
			currentRoom1Shader++;
			if(currentRoom1Shader >2)
				currentRoom1Shader =0;
		}
		if(GUI.Button(new Rect(150, 935, 400, 50), "Switch Texture", bStyle))
		{
			currentRoom1Texture++;
			if(currentRoom1Texture >1)
				currentRoom1Texture =0;
		}
		if(GUI.Button(new Rect(150, 1000, 400, 50), "Switch Object", bStyle))
		{
			currentRoom1Object++;
			if(currentRoom1Object >1)
				currentRoom1Object =0;
		}

		if(currentRoom1Object == 0)
		{
			room1Objects[0].SetActive(true);
			room1Objects[1].SetActive(false);
		}
		else
		{
			room1Objects[0].SetActive(false);
			room1Objects[1].SetActive(true);
		}

		GUI.skin = skin;
		if(currentRoom1Shader == 1)
		{
			GUI.Label(new Rect(1500, 500, 400, 50), "Rim", r2bStyle);
			room1RimP = GUI.HorizontalSlider(new Rect(1500, 550, 400, 100), room1RimP, 0.5f, 6.0f);
			if(GUI.Button(new Rect(1400, 900, 500, 50), "Switch Rim Color", bStyle))
			{
				currentRimColor++;
				if(currentRimColor > 4)
					currentRimColor =0;
			}
		}
		GUI.Label(new Rect(1500, 600, 400, 50), "Glow Power", r2bStyle);
		room1GlowIntensity = GUI.HorizontalSlider(new Rect(1500, 650, 400, 100), room1GlowIntensity, 0.0f, 2.5f);
		GUI.Label(new Rect(1500, 700, 400, 50), "Glow Texture Strength", r2bStyle);
		room1GlowTextureStrength = GUI.HorizontalSlider(new Rect(1500, 750, 400, 100), room1GlowTextureStrength, 0.0f, 10.0f);
		GUI.Label(new Rect(1500, 800, 400, 50), "Glow Width", r2bStyle);
		room1GlowWidth = GUI.HorizontalSlider(new Rect(1500, 850, 400, 100), room1GlowWidth, 0.0f, 0.075f);

		if(GUI.Button(new Rect(1400, 960, 500, 50), "Switch Glow Color", bStyle))
		{
			currentRoom1GlowColor++;
			if(currentRoom1GlowColor > 4)
				currentRoom1GlowColor =0;
		}
		if(GUI.Button(new Rect(1400, 1025, 500, 50), "Switch Glow Texture Color", bStyle))
		{
			currentRoom1GlowTexColor++;
			if(currentRoom1GlowTexColor > 4)
				currentRoom1GlowTexColor =0;
		}

		room1Objects [currentRoom1Object].GetComponent<Renderer>().material.shader = room1Shaders [currentRoom1Shader];

		if(currentRoom1Shader == 2)
			room1Objects [currentRoom1Object].GetComponent<Renderer>().material.SetTexture ("_ToonShade", cm);
		else if(currentRoom1Shader == 1)
		{
			room1Objects [currentRoom1Object].GetComponent<Renderer>().material.SetFloat ("_RimPower", room1RimP);
			room1Objects [currentRoom1Object].GetComponent<Renderer>().material.SetColor ("_RimColor", colors[currentRimColor]);
		}

		room1Objects [currentRoom1Object].GetComponent<Renderer>().material.SetColor ("_MKGlowColor", colors[currentRoom1GlowColor]);
		room1Objects [currentRoom1Object].GetComponent<Renderer>().material.SetColor ("_MKGlowTexColor", colors[currentRoom1GlowTexColor]);
		room1Objects [currentRoom1Object].GetComponent<Renderer>().material.SetTexture ("_MKGlowTex", room2Tex [currentRoom1Texture]);
		room1Objects [currentRoom1Object].GetComponent<Renderer>().material.SetFloat ("_MKGlowPower",room1GlowIntensity);
		room1Objects [currentRoom1Object].GetComponent<Renderer>().material.SetFloat ("_MKGlowTexStrength", room1GlowTextureStrength);
		room1Objects [currentRoom1Object].GetComponent<Renderer>().material.SetFloat ("_MKGlowOffSet", room1GlowWidth);

		string[] shaderName = new string[3] {"Diffuse", "Rim", "Toon"};
		GUI.Label (new Rect (900, 900, 300, 100), shaderName [currentRoom1Shader], r2bStyle);
	}
	private void ManageRoom2()
	{
		InitRoom1 ();
		InitGlowSystem ();
		GUI.Label(new Rect(150, 800, 500, 50), "DiffuseRim", r2bStyle);
		GUI.Label(new Rect(150, 850, 500, 50), "Diffuse", r2bStyle);

		GUI.Label(new Rect(450, 800, 500, 50), "DiffuseRim", r2bStyle);
		GUI.Label(new Rect(450, 850, 500, 50), "Diffuse", r2bStyle);

		GUI.Label(new Rect(800, 800, 500, 50), "ToonBasic", r2bStyle);
		GUI.Label(new Rect(800, 850, 500, 50), "Diffuse", r2bStyle);

		GUI.Label(new Rect(1150, 800, 500, 50), "Unlit", r2bStyle);
		GUI.Label(new Rect(1150, 850, 500, 50), "Diffuse", r2bStyle);

		GUI.Label(new Rect(1500, 800, 500, 50), "Transparent", r2bStyle);
		GUI.Label(new Rect(1500, 850, 500, 50), "Diffuse", r2bStyle);

		GUI.Label(new Rect(100, 950, 400, 50), "Some Variations ", r2bStyle);
		GUI.Label(new Rect(100, 1000, 500, 50), "Every Sphere has the same Textures", r2bStyle);

		if(GUI.Button(new Rect(1500, 950, 400, 50), "Switch Texture", bStyle))
		{
			currentRoom2Texture++;
			if(currentRoom2Texture > 1)
				currentRoom2Texture = 0;

			for(int i = 0; i < 10; i++)
			{
				room2Objects[i].GetComponent<Renderer>().material.SetTexture("_MKGlowTex",room2Tex[currentRoom2Texture]);
			}
		}
	}
}
