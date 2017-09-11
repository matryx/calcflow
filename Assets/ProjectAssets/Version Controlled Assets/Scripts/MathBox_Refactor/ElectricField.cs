using UnityEngine;
using System.Collections;
using System.Collections.Generic;

internal class ElectricCharge
{
    internal Vector3 position;
    internal float charge;

    internal ElectricCharge(Vector3 pos, float c)
    {
        position = pos;
        charge = c;
    }
}

public class ElectricField : MonoBehaviour {

    public Transform positiveChargePrefab;
    List<Transform> positiveCharges;
    public Transform negativeChargePrefab;
    List<Transform> negativeCharges;

    //ParticleSystem shuriken;
    ParticleSystem.Particle[] points;

    public Transform vPrefab;

    public Transform divCubePrefab;
    private Transform divergenceCube;

    List<ElectricCharge> charges;

    List<Vector3> startPts;
    List<Vector3> electricForces;
    float max_force = Mathf.Epsilon;

    Vector3 ElectricFieldFunction(Vector3 position, ElectricCharge e)
    {
        Vector3 direction = position - e.position;
        float r_square = direction.sqrMagnitude;
        direction = direction.normalized;
        return direction * e.charge / r_square;
    }

	// Use this for initialization
	void Start () {
        //shuriken = GetComponent<ParticleSystem>();
        points = new ParticleSystem.Particle[1000];
        positiveCharges = new List<Transform>();
        negativeCharges = new List<Transform>();

        startPts = new List<Vector3>();
        electricForces = new List<Vector3>();

        charges = new List<ElectricCharge>();
        /*
        ElectricCharge e = new ElectricCharge(new Vector3(0, 5, 0), 1f);
        charges.Add(e);
        e = new ElectricCharge(new Vector3(0, -5, 0), -1f);
        //charges.Add(e);
        
        e = new ElectricCharge(new Vector3(5, 0, 0), 1f);
        charges.Add(e);
        e = new ElectricCharge(new Vector3(-5, 0, 0), 1f);
        charges.Add(e);
        e = new ElectricCharge(new Vector3(0, 0, 0), -4f);
        charges.Add(e);
        e = new ElectricCharge(new Vector3(10, 10, 0), -4f);
        charges.Add(e);
        e = new ElectricCharge(new Vector3(-10, 10, 0), -4f);
        charges.Add(e);
        e = new ElectricCharge(new Vector3(10, -10, 0), -4f);
        //charges.Add(e);
        e = new ElectricCharge(new Vector3(-10, -10, 0), -4f);
        //charges.Add(e);
        */

        //GenerateStartPoints();
        //CaculateElectricForces();
        //DrawVectorField();
        divergenceCube = Instantiate(divCubePrefab);

        GenrateElectricField();
        CalculateDivergence();
	}
	
	// Update is called once per frame
	void Update () {
        //AnimateParticleFlow();
        AnimateElectricField();
        CalculateDivergence();
	}

    public void MoveDivergenceBox(Vector3 position)
    {
        divergenceCube.position = position;
    }

    public void AddCharge(Vector3 position, float charge)
    {
        ElectricCharge e = new ElectricCharge(position, charge);
        charges.Add(e);
        if (charge > 0)
        {
            Transform sphere = (Transform)Instantiate(positiveChargePrefab, e.position, Quaternion.identity);
            sphere.SetParent(transform, false);
            positiveCharges.Add(sphere);
        }
        else
        {
            Transform sphere = (Transform)Instantiate(negativeChargePrefab, e.position, Quaternion.identity);
            sphere.SetParent(transform, false);
            negativeCharges.Add(sphere);
        }
    }

    void GenerateStartPoints()
    {
        for(float x = -3.5f; x <= 3.5f; x+=1f)
        {
            for(float y = -3.5f; y <= 3.5f; y+=1f)
            {
                for(float z = -3.5f; z <= 3.5f; z+=1f)
                {
                    startPts.Add(new Vector3(x, y, z));
                }
            }
        }
    }

    void CaculateElectricForces()
    {
        for(int i = 0; i < startPts.Count; i++)
        {
            Vector3 f = new Vector3(0, 0, 0);
            foreach(ElectricCharge e in charges)
            {
                f += ElectricFieldFunction(startPts[i], e);
            }
            electricForces.Add(f);
            if(f.magnitude > max_force)
            {
                max_force = f.magnitude;
            }
        }
    }

    public void DrawVectorField()
    {
        for (int i = 0; i < startPts.Count; i++)
        {
            Vector3 target = startPts[i];
            Vector3 offset = electricForces[i] / max_force;
            Vector3 tip = offset * 0.4f;

            Transform l = Instantiate(vPrefab);
            l.SetParent(transform, false);
            LineRenderer top = l.Find("Top").GetComponent<LineRenderer>();
            top.SetPosition(0, target + offset - tip);
            top.SetPosition(1, target + offset);
            LineRenderer body = l.Find("Body").GetComponent<LineRenderer>();
            body.SetPosition(0, target + offset - tip);
            body.SetPosition(1, target);
            //vectors.Add(l);
        }
    }

    void AnimateParticleFlow()
    {
        //shuriken.GetParticles(points);

        for(int i = 0; i < points.Length; i++)
        {
            Vector3 f = Vector3.zero;
            foreach (ElectricCharge e in charges)
            {
                f += ElectricFieldFunction(points[i].position, e);
            }
            points[i].velocity = f / max_force * 5f;
        }
        //shuriken.SetParticles(points, points.Length);
    }

    void GenrateElectricField()
    {
        foreach(ElectricCharge e in charges)
        {
            if (e.charge > 0) {
                Transform sphere = (Transform)Instantiate(positiveChargePrefab, e.position, Quaternion.identity);
                sphere.SetParent(transform, false);
                positiveCharges.Add(sphere);
            }
            else
            {
                Transform sphere = (Transform)Instantiate(negativeChargePrefab, e.position, Quaternion.identity);
                sphere.SetParent(transform, false);
                negativeCharges.Add(sphere);
            }
        }

    }

    public void Clear()
    {
        foreach(Transform t in positiveCharges)
        {
            Destroy(t.gameObject);
        }
        foreach(Transform t in negativeCharges)
        {
            Destroy(t.gameObject);
        }
        negativeCharges.Clear();
        positiveCharges.Clear();
        charges.Clear();
    }

    void AnimateElectricField()
    {
        foreach (Transform p in positiveCharges)
        {
            ParticleSystem shuriken = p.GetComponent<ParticleSystem>();
            shuriken.GetParticles(points);
            for(int i = 0; i < points.Length; i++)
            {
                Vector3 f = Vector3.zero;
                foreach (ElectricCharge e in charges)
                {
                    f += ElectricFieldFunction(points[i].position, e);
                }
                points[i].velocity = f * 10f;
            }
            shuriken.SetParticles(points, points.Length);
        }
        foreach (Transform p in negativeCharges)
        {
            ParticleSystem shuriken = p.GetComponent<ParticleSystem>();
            shuriken.GetParticles(points);
            for (int i = 0; i < points.Length; i++)
            {
                Vector3 f = Vector3.zero;
                foreach (ElectricCharge e in charges)
                {
                    f += ElectricFieldFunction(points[i].position, e);
                }
                points[i].velocity = f * 10f;
            }
            shuriken.SetParticles(points, points.Length);
        }
    }

    void CalculateDivergence()
    {
        //divergenceCube = Instantiate(divCubePrefab);
        Transform[] children = divergenceCube.GetComponentsInChildren<Transform>();
        foreach(Transform child in children)
        {
            child.gameObject.SetActive(false);
        }
        divergenceCube.gameObject.SetActive(true);

        Vector3 cube_center = divergenceCube.position;
        //divergenceCube.position = cube_center;
        Vector3 x_pos = cube_center + new Vector3(0.5f, 0, 0);
        Vector3 f = Vector3.zero;
        foreach (ElectricCharge e in charges)
        {
            f += ElectricFieldFunction(x_pos, e);
        }
        if(Vector3.Dot(f, new Vector3(.5f, 0, 0)) < 0)
        {
            divergenceCube.Find("X_Positive_INV").gameObject.SetActive(true);
            //divergenceCube.FindChild("X_Positive_NEU").gameObject.SetActive(false);
            //divergenceCube.FindChild("X_Positive").gameObject.SetActive(false);
        }
        else if(Vector3.Dot(f, new Vector3(.5f, 0, 0)) == 0)
        {
            divergenceCube.Find("X_Positive_NEU").gameObject.SetActive(true);
        }
        else
        {
            divergenceCube.Find("X_Positive").gameObject.SetActive(true);
        }

        Vector3 x_neg = cube_center - new Vector3(0.5f, 0, 0);
        f = Vector3.zero;
        foreach (ElectricCharge e in charges)
        {
            f += ElectricFieldFunction(x_neg, e);
        }
        if (Vector3.Dot(f, new Vector3(-0.5f, 0, 0)) < 0)
        {
            divergenceCube.Find("X_Negative_INV").gameObject.SetActive(true);
        }
        else if (Vector3.Dot(f, new Vector3(-.5f, 0, 0)) == 0)
        {
            divergenceCube.Find("X_Negative_NEU").gameObject.SetActive(true);
        }
        else
        {
            divergenceCube.Find("X_Negative").gameObject.SetActive(true);
        }

        Vector3 y_pos = cube_center + new Vector3(0, 0.5f, 0);
        f = Vector3.zero;
        foreach (ElectricCharge e in charges)
        {
            f += ElectricFieldFunction(y_pos, e);
        }
        if (Vector3.Dot(f, new Vector3(0, 0.5f, 0)) < 0)
        {
            divergenceCube.Find("Y_Positive_INV").gameObject.SetActive(true);
        }
        else if (Vector3.Dot(f, new Vector3(0,.5f, 0)) == 0)
        {
            divergenceCube.Find("Y_Positive_NEU").gameObject.SetActive(true);
        }
        else
        {
            divergenceCube.Find("Y_Positive").gameObject.SetActive(true);
        }

        Vector3 y_neg = cube_center - new Vector3(0, 0.5f, 0);
        f = Vector3.zero;
        foreach (ElectricCharge e in charges)
        {
            f += ElectricFieldFunction(y_neg, e);
        }
        if (Vector3.Dot(f, new Vector3(0, -0.5f, 0)) < 0)
        {
            divergenceCube.Find("Y_Negative_INV").gameObject.SetActive(true);
        }
        else if (Vector3.Dot(f, new Vector3(0, -.5f, 0)) == 0)
        {
            divergenceCube.Find("Y_Negative_NEU").gameObject.SetActive(true);
        }
        else
        {
            divergenceCube.Find("Y_Negative").gameObject.SetActive(true);
        }

        Vector3 z_pos = cube_center + new Vector3(0, 0, 0.5f);
        f = Vector3.zero;
        foreach (ElectricCharge e in charges)
        {
            f += ElectricFieldFunction(z_pos, e);
        }
        if (Vector3.Dot(f, new Vector3(0, 0, 0.5f)) < 0)
        {
            divergenceCube.Find("Z_Positive_INV").gameObject.SetActive(true);
        }
        else if (Vector3.Dot(f, new Vector3(0, 0,.5f)) == 0)
        {
            divergenceCube.Find("Z_Positive_NEU").gameObject.SetActive(true);
        }
        else
        {
            divergenceCube.Find("Z_Positive").gameObject.SetActive(true);
        }

        Vector3 z_neg = cube_center - new Vector3(0, 0, 0.5f);
        f = Vector3.zero;
        foreach (ElectricCharge e in charges)
        {
            f += ElectricFieldFunction(z_neg, e);
        }
        if (Vector3.Dot(f, new Vector3(0, 0, -0.5f)) < 0)
        {
            divergenceCube.Find("Z_Negative_INV").gameObject.SetActive(true);
        }
        else if (Vector3.Dot(f, new Vector3(0, 0,-.5f)) == 0)
        {
            divergenceCube.Find("Z_Negative_NEU").gameObject.SetActive(true);
        }
        else
        {
            divergenceCube.Find("Z_Negative").gameObject.SetActive(true);
        }

    }
}
