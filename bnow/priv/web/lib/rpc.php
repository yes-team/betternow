<?PHP
Class rpc{
  protected function request($path, $params, $method = 'post')
  {
    $method = strtolower($method);
    try {
      switch ($method) {
        case 'get':
          $signParams = $this->sign($method, $path, $params, array());
          $response = @$this->client->get($this->request_uri . $path . '?' . $signParams);
          break;
        case 'post':
        case 'delete':
        case 'put':
          $signParams = $this->sign($method, $path, array(), $params);
          $body = http_build_query($params);
         $client = new Buzz\Browser( new Buzz\Client\Curl);
          $response = $client->{$method}(OPENAPI_URI . $path . '?' . $signParams, array(), $body);
        default:
          break;
      }
    } catch (Exception $e) {
      $this->error = $e->getMessage();
    }

    $this->response = $response;
    if ($response->isSuccessful()) {
      $content = json_decode($response->getContent(),true);
      if (isset($content->message)) {
        $this->error = $content->message;
      }
    }
    return isset($content) ? $content : null;
  }

  protected function sign($method, $path, $get, $post)
  {
    $signParams = array(
      'client_id' => OPENAPI_KEY,
      'sign_method' => 'md5',
      'sign_time' => time(),
    );

    $get = array_merge($signParams, $get);

    ksort($get);
    ksort($post);

    $url = parse_url(OPENAPI_URI. $path);
 
    $sign = array(
      OPENAPI_SECRET,
      strtoupper($method),
      rawurlencode($url['path']),
      '',
      rawurlencode(urldecode(http_build_query($get))),
      rawurlencode(urldecode(http_build_query($post))),
      OPENAPI_SECRET
    );

    $get['sign'] = strtoupper(md5(join('&', $sign)));

    return http_build_query($get);
  }

}
