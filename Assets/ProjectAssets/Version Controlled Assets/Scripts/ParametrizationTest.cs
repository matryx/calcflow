using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ParametrizationTest : MonoBehaviour {
    Parametrization param;

    ParticleSystem shuriken;
    ParticleSystem.Particle[] particles;
	// Use this for initialization
	void Start () {
        param = new Parametrization();
        param.Initialize();

        //param.SetExpr1("(cos(u)+sin(v))*sin(w)+cos(t)");
        //param.SetExpr2("(cos(v)+sin(u))*sin(w)+cos(t)");
        //param.SetExpr3("cos(w)+sin(t)");
        param.SetExpr1("w*sin(u)*cos(v)+cos(t*15)*0.3");
        param.SetExpr2("w*cos(u)*cos(v)+cos(t*15)*0.3");
        param.SetExpr3("w*sin(v)");

        param.AddParameter("u");
        param.SetParameterMin("u", "0");param.SetParameterMax("u", "pi");
        param.AddParameter("v");
        param.SetParameterMin("v", "0");param.SetParameterMax("v", "2*pi");
        param.AddParameter("w");
        param.SetParameterMin("w", "0");param.SetParameterMax("w", "5");
        param.AddParameter("t");
        param.SetParameterMin("t", "0"); param.SetParameterMax("t", "2*pi");

        param.SetupSolver();
        param.SetupSamples();
        List<Vector3> positions = param.Evaluate();

        particles = new ParticleSystem.Particle[positions.Count];

        for(int i = 0; i < positions.Count; i++)
        {
            particles[i].position = positions[i];
            particles[i].startSize = 0.05f;
            particles[i].startColor = Color.cyan;
        }

        shuriken = GetComponent<ParticleSystem>();
        
        shuriken.SetParticles(particles, positions.Count);
        Debug.Log(shuriken.particleCount);
    }
	
	// Update is called once per frame
	void Update () {
		
	}
}
