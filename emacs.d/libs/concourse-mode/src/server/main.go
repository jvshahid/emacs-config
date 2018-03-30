package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"net"
	"net/http"
	"os"

	"github.com/tedsuo/rata"
)

// routes:
// /teams/:team/pipelines/:pipeline/jobs/:job/builds/:buildno

func getFile(path string) http.Handler {
	return http.HandlerFunc(func(rw http.ResponseWriter, req *http.Request) {
		bs, err := ioutil.ReadFile(path)
		if err != nil {
			rw.WriteHeader(http.StatusInternalServerError)
			return
		}
		rw.Write(bs)
	})
}

func main() {
	port := flag.Int("port", 0, "port to listen on")
	pipelineJson := flag.String("pipeline-response", fmt.Sprintf("%s/fixtures/jobs-formatted.json", os.Getenv("GOPATH")), "path to the pipeline json response ")
	jobJson := flag.String("job-response", fmt.Sprintf("%s/fixtures/builds-formatted.json", os.Getenv("GOPATH")), "path to the pipeline json response ")
	certFile := flag.String("cert", fmt.Sprintf("%s/fixtures/certs/localhost.crt", os.Getenv("GOPATH")), "path to the cert file ")
	keyFile := flag.String("key", fmt.Sprintf("%s/fixtures/certs/localhost.key", os.Getenv("GOPATH")), "path to the key file ")
	flag.Parse()

	routes := rata.Routes{
		{Name: "pipeline", Method: rata.GET, Path: "/api/v1/teams/:name/pipelines/:name/jobs"},
		{Name: "job", Method: rata.GET, Path: "/api/v1/teams/:name/pipelines/:name/jobs/:job/builds"},
	}

	handlers := rata.Handlers{
		"pipeline": getFile(*pipelineJson),
		"job":      getFile(*jobJson),
	}

	router, err := rata.NewRouter(routes, handlers)
	if err != nil {
		panic(err)
	}

	listener, err := net.Listen("tcp", fmt.Sprintf(":%d", *port))
	if err != nil {
		panic(err)
	}
	fmt.Printf("listening on %s\n", listener.Addr())
	err = http.ServeTLS(listener, router, *certFile, *keyFile)
	if err != nil {
		panic(err)
	}
	os.Exit(0)
}
